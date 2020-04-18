{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind -fno-warn-name-shadowing #-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

import Control.Lens hiding (indices, transform)
import Linear

import Graphics.GL.Core33 as GL
import Graphics.GL.Types  as GL
import Graphics.UI.GLFW   as GLFW hiding (getTime, getCursorPos)

import Data.Word

import GLStorable
import Lib
import NonGL
import Types
import Window

{-# ANN module "hlint: ignore Redundant bracket" #-}

data AppEnv = AppEnv
  { _appVao     :: VAO
  , _appEbo     :: Buffer
  , _appMdl     :: Uniform (M44 Float)
  , _appView    :: Uniform (M44 Float)
  -- , appTex2 :: GLuint
  , _appProgram :: Program
  }

data AppState = AppState
  { _pos   :: V3 Float
  , _pitch :: Float
  , _yaw   :: Float
  , _cursorPrev :: (Double, Double)
  }

makeLenses ''AppState

vertices :: [V3 Float :. V2 Float]
vertices =
  [ V3 (-0.5) (-0.5) (-0.5) :. V2 0.0 0.0
  , V3 ( 0.5) (-0.5) (-0.5) :. V2 1.0 0.0
  , V3 ( 0.5) ( 0.5) (-0.5) :. V2 1.0 1.0
  , V3 ( 0.5) ( 0.5) (-0.5) :. V2 1.0 1.0
  , V3 (-0.5) ( 0.5) (-0.5) :. V2 0.0 1.0
  , V3 (-0.5) (-0.5) (-0.5) :. V2 0.0 0.0

  , V3 (-0.5) (-0.5) ( 0.5) :. V2 0.0 0.0
  , V3 ( 0.5) (-0.5) ( 0.5) :. V2 1.0 0.0
  , V3 ( 0.5) ( 0.5) ( 0.5) :. V2 1.0 1.0
  , V3 ( 0.5) ( 0.5) ( 0.5) :. V2 1.0 1.0
  , V3 (-0.5) ( 0.5) ( 0.5) :. V2 0.0 1.0
  , V3 (-0.5) (-0.5) ( 0.5) :. V2 0.0 0.0

  , V3 (-0.5) ( 0.5) ( 0.5) :. V2 1.0 0.0
  , V3 (-0.5) ( 0.5) (-0.5) :. V2 1.0 1.0
  , V3 (-0.5) (-0.5) (-0.5) :. V2 0.0 1.0
  , V3 (-0.5) (-0.5) (-0.5) :. V2 0.0 1.0
  , V3 (-0.5) (-0.5) ( 0.5) :. V2 0.0 0.0
  , V3 (-0.5) ( 0.5) ( 0.5) :. V2 1.0 0.0

  , V3 ( 0.5) ( 0.5) ( 0.5) :. V2 1.0 0.0
  , V3 ( 0.5) ( 0.5) (-0.5) :. V2 1.0 1.0
  , V3 ( 0.5) (-0.5) (-0.5) :. V2 0.0 1.0
  , V3 ( 0.5) (-0.5) (-0.5) :. V2 0.0 1.0
  , V3 ( 0.5) (-0.5) ( 0.5) :. V2 0.0 0.0
  , V3 ( 0.5) ( 0.5) ( 0.5) :. V2 1.0 0.0

  , V3 (-0.5) (-0.5) (-0.5) :. V2 0.0 1.0
  , V3 ( 0.5) (-0.5) (-0.5) :. V2 1.0 1.0
  , V3 ( 0.5) (-0.5) ( 0.5) :. V2 1.0 0.0
  , V3 ( 0.5) (-0.5) ( 0.5) :. V2 1.0 0.0
  , V3 (-0.5) (-0.5) ( 0.5) :. V2 0.0 0.0
  , V3 (-0.5) (-0.5) (-0.5) :. V2 0.0 1.0

  , V3 (-0.5) ( 0.5) (-0.5) :. V2 0.0 1.0
  , V3 ( 0.5) ( 0.5) (-0.5) :. V2 1.0 1.0
  , V3 ( 0.5) ( 0.5) ( 0.5) :. V2 1.0 0.0
  , V3 ( 0.5) ( 0.5) ( 0.5) :. V2 1.0 0.0
  , V3 (-0.5) ( 0.5) ( 0.5) :. V2 0.0 0.0
  , V3 (-0.5) ( 0.5) (-0.5) :. V2 0.0 1.0
  ]

indices :: [Word32]
indices =
  [ 0, 1, 3
  , 1, 2, 3
  ]

cubePositions :: [V3 Float]
cubePositions =
  [ V3 ( 0.0) ( 0.0) ( 0.0)
  , V3 ( 2.0) ( 5.0) (-15.0)
  , V3 (-1.5) (-2.2) (-2.5)
  , V3 (-3.8) (-2.0) (-12.3)
  , V3 ( 2.4) (-0.4) (-3.5)
  , V3 (-1.7) ( 3.0) (-7.5)
  , V3 ( 1.3) (-2.0) (-2.5)
  , V3 ( 1.5) ( 2.0) (-2.5)
  , V3 ( 1.5) ( 0.2) (-1.5)
  , V3 (-1.3) ( 1.0) (-1.5)
  ]

modelM, viewM, projectionM :: M44 Float
modelM = mkTransformation (axisAngle (unit _x) (-55/180*pi)) 0
viewM  = identity & translation .~ V3 0 0 (-3)
projectionM  = perspective (45/180*pi) (640/480) 0.1 100

cameraSpeed :: Float
cameraSpeed = 2.5

main :: IO ()
main = withWindow defaultHints $ do
    hideCursor
    env <- either (error . showShaderError) id <$> buildEnvironment
    p <- getCursorPos
    loop env (AppState 0 0 0 p)
    pure ()
  where
    buildEnvironment :: WindowT IO (Either ShaderError AppEnv)
    buildEnvironment = do

      vao <- genArray
      bindArray vao

      vbo <- genBuffer
      bufferData vbo vertices GL_ARRAY_BUFFER GL_STATIC_DRAW

      ebo <- genBuffer
      bufferData ebo indices GL_ELEMENT_ARRAY_BUFFER GL_STATIC_DRAW

      setVertexAttribs vertices

      let setParams = liftIO$ do
            glGenerateMipmap GL_TEXTURE_2D
            glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_MIRRORED_REPEAT
            glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_MIRRORED_REPEAT
            glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR_MIPMAP_LINEAR -- Only for MIN_FILTER
            glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST

      glActiveTexture GL_TEXTURE0
      Right _ <- loadTexture2D "awesomeface.png" False True
      setParams
      glActiveTexture GL_TEXTURE1
      Right _ <- loadTexture2D "container.jpg" False True
      setParams

      eprog <- createShaderProgram
        [ ("glsl/triangle.vert", GL_VERTEX_SHADER)
        , ("glsl/triangle.frag", GL_FRAGMENT_SHADER)
        ]
      forM eprog $ \prog -> do
        glClearColor 0.2 0.3 0.3 1.0
        glPolygonMode GL_BACK GL_LINE
        useProgram prog
        setUniformName prog "texFace" (0 :: GLint)
        setUniformName prog "texBox" (1 :: GLint)
        Just mdl  <- setUniformName prog "model" modelM
        Just view <- setUniformName prog "view" viewM
        setUniformName prog "projection" projectionM
        glEnable GL_DEPTH_TEST
        pure (AppEnv vao ebo mdl view prog)

    loop (AppEnv vao _ mdl view _) sInit =
      flip runStateT sInit $ do
        pos   .= V3 0 0 3
        -- front .= -unit _z
        -- up    .= unit _y
        yaw .= (-90/180*pi)
        bufferSwapLoop $ \dt -> do

          () <- do
            (cx',cy') <- use cursorPrev
            c@(cx,cy) <- getCursorPos
            cursorPrev .= c
            yaw   -= realToFrac (cx-cx') /180
            pitch += realToFrac (cy-cy') /180
            pitch %= min 1 . max (-1)
            -- pitch %= const 0

          front <- do
            p <- use pitch
            y <- use yaw
            pure $ normalize $
              V3 (cos y * cos p)
                 (sin p)
                 (sin y * cos p)

          x <- use pos
          liftIO $ do
            putStrLn $ "front: " <> show front
            putStrLn $ "pos:   " <> show x
          let up = cross front (cross front (unit _y))

          time <- getTime
          let camSpeed = cameraSpeed * dt
          let mdlM pos speed = mkTransformation (axisAngle (V3 0.5 1 0) (time * speed / 9 * pi)) pos :: M44 Float

          whenM (isKeyPressed Key'W)           $ pos += camSpeed *^ front
          whenM (isKeyPressed Key'S)           $ pos -= camSpeed *^ front
          whenM (isKeyPressed Key'A)           $ pos -= camSpeed *^ normalize (cross front up)
          whenM (isKeyPressed Key'D)           $ pos += camSpeed *^ normalize (cross front up)
          whenM (isKeyPressed Key'Space)       $ pos += camSpeed *^ up
          whenM (isKeyPressed Key'LeftControl) $ pos -= camSpeed *^ up

          quit <- fmap or . mapM isKeyPressed $ [Key'Escape, Key'Q]

          x <- use pos
          bindArray vao
          setUniform view $ lookAt x (x+front) up
          forM_ (zip cubePositions [1..]) $ \ (pos, i) -> do
            setUniform mdl (mdlM pos i)
            -- glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_INT nullPtr
            glDrawArrays GL_TRIANGLES 0 36

          glBindVertexArray 0
          return quit
