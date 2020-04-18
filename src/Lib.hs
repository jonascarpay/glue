{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Lib where

import Control.Lens
import Control.Monad
import Control.Monad.Except

import Foreign          as F
import Foreign.C.String as F

import Linear hiding (norm)

import Codec.Picture        as Pic hiding (Uniform)
import Data.Vector.Storable (unsafeWith)
import Data.Maybe (fromJust)

import Graphics.GL.Core33 as GL
import Graphics.GL.Types  as GL
import Graphics.UI.GLFW   as GLFW

import Types
import Window
import GLStorable

data ShaderError
  = ShaderObjectError
  | ShaderCompilationError FilePath String
  | ProgramLinkError String

showShaderError :: ShaderError -> String
showShaderError ShaderObjectError = "Error while creating shader object"
showShaderError (ShaderCompilationError path err) =
  "Error while compiling " <> path <> ":\n" <> err
showShaderError (ProgramLinkError err) =
  "Error while linking:\n" <> err

-- TODO -- infer shaderType from file extension
createShader :: MonadIO m => FilePath -> GLenum -> m (Either ShaderError GLenum)
createShader filename shaderType = liftIO $ do
  shaderId <- glCreateShader shaderType
  if shaderId == 0
     then pure $ Left ShaderObjectError
     else do
       shaderSrc <- readFile filename
       F.withCString shaderSrc $ \ src' ->
         F.with src' $ \ src'' ->
           F.alloca $ \success -> do
             glShaderSource shaderId 1 src'' F.nullPtr
             glCompileShader shaderId
             glGetShaderiv shaderId GL_COMPILE_STATUS success
             wasSuccess <- F.peek success
             let resultBytes = 512
             if wasSuccess == 0
                then F.allocaBytes resultBytes $ \res -> do
                  glGetShaderInfoLog shaderId (fromIntegral resultBytes) F.nullPtr res
                  str <- F.peekCString res
                  pure . Left $ ShaderCompilationError filename str
                else pure $ Right shaderId

createShaderProgram :: MonadIO m => [(FilePath, GLenum)] -> m (Either ShaderError Program)
createShaderProgram shaders = liftIO $ do
  eShaders <- forM shaders $ uncurry createShader
  case sequence eShaders of
    Left err -> pure $ Left err
    Right shaders' -> do
      programId <- glCreateProgram
      forM_ shaders' $ glAttachShader programId
      glLinkProgram programId
      F.alloca $ \ success -> do
        glGetProgramiv programId GL_LINK_STATUS success
        wasSuccess <- F.peek success
        let resultBytes = 512
        if wasSuccess == 0
          then F.allocaBytes resultBytes $ \res -> do
            glGetProgramInfoLog programId (fromIntegral resultBytes) F.nullPtr res
            str <- F.peekCString res
            pure . Left $ ProgramLinkError str
          else do
            forM_ shaders' glDeleteShader
            pure $ Right $ Program programId

-- TODO waarom bestaat dit niet?
withArraySize :: forall e a. F.Storable e => [e] -> (GLsizeiptr -> F.Ptr e -> IO a) -> IO a
withArraySize arr f = F.withArrayLen arr $
  \len arr' -> f (fromIntegral $ len * F.sizeOf (error "touched arraySize" :: e)) arr'

genArray :: MonadWindow m => m VAO
genArray = liftIO$ alloca $ \ vao' -> do
  glGenVertexArrays 1 vao'
  VAO <$> peek vao'

bindArray :: MonadWindow m => VAO -> m ()
bindArray (VAO vao) = liftIO$ glBindVertexArray vao

genBuffer :: MonadWindow m => m Buffer
genBuffer = liftIO$ alloca $ \buf' -> do
  glGenBuffers 1 buf'
  Buffer <$> peek buf'

bufferData :: (Storable e, MonadWindow m) => Buffer -> [e] -> GLenum -> GLenum -> m ()
bufferData (Buffer buffer) bufferdata buffertype drawtype = liftIO$
  withArraySize bufferdata $ \len arr -> do
    glBindBuffer buffertype buffer
    glBufferData buffertype len (castPtr arr) drawtype

newtype UniformError = UniformNotFound String

getUniform :: MonadIO m => String -> Program -> m (Either UniformError (Uniform a))
getUniform name (Program program) = liftIO $ do
  uni <- withCString name $ \ ptr' ->
    glGetUniformLocation program ptr'
  return $ case uni of
    -1 -> Left $ UniformNotFound name
    n  -> Right $ Uniform n

setUniformName :: (MonadIO m, GLUniform a) => Program -> String -> a -> m (Maybe (Uniform a))
setUniformName prog name val = do
  ename <- getUniform name prog
  case ename of
    Right uni -> setUniform uni val >> pure (Just uni)
    Left _    -> pure Nothing

-- Assumes all things are size 4.
-- Is that OK?
setVertexAttribs :: forall m p a. (MonadWindow m, GLAttrib a) => p a -> m ()
setVertexAttribs _ = liftIO $ go attribList 0 0
  where
    attribList = attribs (undefined :: a)
    stride = fromIntegral . (*4) . sum $ fmap (\(s,_,_) -> s) attribList
    go [] _ _ = return ()
    go ((size,gltype,norm):as) n off = do
      glVertexAttribPointer n size gltype norm stride (intPtrToPtr $ fromIntegral off)
      glEnableVertexAttribArray n
      go as (n + 1) (off + size * 4)

newtype TextureError
  = ImgLoadError String

loadTexture2D :: MonadIO m => FilePath -> Bool -> Bool -> m (Either TextureError Texture)
loadTexture2D path flipX flipY = liftIO$ do
  tex <- alloca $ \ tex' -> do
    glGenTextures 1 tex'
    peek tex'
  glBindTexture GL_TEXTURE_2D tex
  mImg <- fmap (flipImg flipX flipY . convertRGB8) <$> readImage path
  case mImg of
    Left err -> pure . Left $ ImgLoadError err
    Right (Image imgW imgH imgData) -> do
      unsafeWith imgData $ \imgPtr ->
        glTexImage2D
          GL_TEXTURE_2D 0 GL_RGB
          (fromIntegral imgW) (fromIntegral imgH)
          0 GL_RGB GL_UNSIGNED_BYTE (castPtr imgPtr)
      pure $ Right $ Texture tex

getTime :: MonadWindow m => m Float
getTime = liftIO$ realToFrac . fromJust <$> GLFW.getTime

flipImg :: Pixel a => Bool -> Bool -> Pic.Image a -> Pic.Image a
flipImg flipX flipY img = generateImage f (imageWidth img) (imageHeight img)
  where
    w' = imageWidth img - 1
    h' = imageHeight img - 1
    f x y = pixelAt img (if flipX then w' - x else x) (if flipY then h' - y else y)

translate :: Num a => V3 a -> M44 a
translate v = identity & translation .~ v

useProgram :: MonadIO m => Program -> m ()
useProgram = glUseProgram . unProgram
