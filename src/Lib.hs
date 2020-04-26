{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Control.Lens
import Control.Monad.Except
import Data.Maybe (fromJust)
import Foreign as F
import GLStorable
import Graphics.GL.Core33 as GL
import Graphics.GL.Types as GL
import Graphics.UI.GLFW as GLFW
import Linear hiding (norm)
import Types
import Window

-- TODO waarom bestaat dit niet?
withArraySize :: forall e a. F.Storable e => [e] -> (GLsizeiptr -> F.Ptr e -> IO a) -> IO a
withArraySize arr f = F.withArrayLen arr $
  \len arr' -> f (fromIntegral $ len * F.sizeOf (error "touched arraySize" :: e)) arr'

genArray :: MonadWindow m => m VAO
genArray = liftIO $ alloca $ \vao' -> do
  glGenVertexArrays 1 vao'
  VAO <$> peek vao'

bindArray :: MonadWindow m => VAO -> m ()
bindArray (VAO vao) = liftIO $ glBindVertexArray vao

unbindArray :: MonadWindow m => m ()
unbindArray = bindArray (VAO 0)

genBuffer :: MonadWindow m => m Buffer
genBuffer = liftIO $ alloca $ \buf' -> do
  glGenBuffers 1 buf'
  Buffer <$> peek buf'

-- Also binds buffer
bufferData :: (Storable e, MonadWindow m) => Buffer -> [e] -> GLenum -> GLenum -> m ()
bufferData (Buffer buffer) bufferdata buffertype drawtype = liftIO
  $ withArraySize bufferdata
  $ \len arr -> do
    glBindBuffer buffertype buffer
    glBufferData buffertype len (castPtr arr) drawtype

-- Assumes all things are size 4.
-- Is that OK?
setVertexAttribs :: forall m p a. (MonadWindow m, GLVertex a) => p a -> m ()
setVertexAttribs _ = liftIO $ go attribList 0 0
  where
    attribList = vertexAttribs (undefined :: p a)
    stride = (* 4) . sum . fmap attrSize $ attribList
    go [] _ _ = return ()
    go (VertexAttribs size gltype norm : as) n off = do
      glVertexAttribPointer n size gltype norm stride (intPtrToPtr $ fromIntegral off)
      glEnableVertexAttribArray n
      go as (n + 1) (off + size * 4)

getTime :: MonadWindow m => m Float
getTime = liftIO $ realToFrac . fromJust <$> GLFW.getTime

translate :: Num a => V3 a -> M44 a
translate v = identity & translation .~ v
