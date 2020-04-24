{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind -fno-warn-name-shadowing #-}

import Control.Lens hiding (indices, transform)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import GLStorable
import Graphics.GL.Core33 as GL
import Graphics.UI.GLFW as GLFW hiding (getCursorPos, getTime)
import Lib
import Linear
import Mesh
import NonGL
import Obj
import Types
import Window

data AppEnv
  = AppEnv
      { cubeView :: Uniform (M44 Float),
        cubeModel :: Uniform (M44 Float),
        cubeViewPos :: Uniform (V3 Float),
        cubeProg :: Program,
        meshes :: [GPUMesh]
      }

data AppState
  = AppState
      { _pos :: V3 Float,
        _pitch :: Float,
        _yaw :: Float,
        _cursorPrev :: (Double, Double)
      }

makeLenses ''AppState

lightPos :: V3 Float
lightPos = V3 5 3 0

projectionM :: M44 Float
projectionM = perspective (45 / 180 * pi) (640 / 480) 0.1 100

cameraSpeed :: Float
cameraSpeed = 2.5

main :: IO ()
main = withWindow defaultHints $ do
  -- hideCursor
  (menv, log) <- runWriterT $ runExceptT buildEnvironment
  liftIO $ putStrLn log
  case menv of
    Left err -> liftIO . putStrLn . showShaderError $ err
    Right env -> do
      p <- getCursorPos
      loop env (AppState 0 0 0 p)
      pure ()
  where
    buildEnvironment :: ExceptT ShaderError (WriterT String (WindowT IO)) AppEnv
    buildEnvironment = do
      --
      glEnable GL_DEPTH_TEST
      --
      cubeProg <-
        createShaderProgram
          [ ("glsl/common.vert", GL_VERTEX_SHADER),
            ("glsl/object.frag", GL_FRAGMENT_SHADER)
          ]
      meshes <- do
        objs <-
          liftIO (loadObj "/home/jmc/Downloads/nanosuit/nanosuit.obj")
            >>= either (throwError . OtherError) pure
        traverse setupMesh (toMesh objs)
      --
      do
        useProgram cubeProg -- Can only set after binding
          -- setUniformByName cubeProg "material.shininess" (32 :: Float)
          -- setUniformByName cubeProg "material.diffuse" (0 :: TextureUnit)
          -- setUniformByName cubeProg "material.specular" (1 :: TextureUnit)
        setUniformByName cubeProg "projection" projectionM
      setUniformByName cubeProg "light.ambient" (0.1 :: V3 Float)
      setUniformByName cubeProg "light.diffuse" (1 :: V3 Float)
      setUniformByName cubeProg "light.specular" (1 :: V3 Float)
      -- setUniformByName cubeProg "light.direction" (- unit _y :: V3 Float)
      setUniformByName cubeProg "light.position" lightPos
      cubeModel <- getUniform cubeProg "model"
      cubeView <- getUniform cubeProg "view"
      cubeViewPos <- getUniform cubeProg "viewPos"
      pure AppEnv {..}
    loop AppEnv {..} sInit =
      flip runStateT sInit $ do
        pos .= V3 0 0 6
        yaw .= (-90 / 180 * pi)
        bufferSwapLoop $ \dt -> do
          do
            let sensitivity = 0.1
            (cx', cy') <- use cursorPrev
            c@(cx, cy) <- getCursorPos
            cursorPrev .= c
            yaw += realToFrac (cx - cx') / 180 * pi * sensitivity
            pitch -= realToFrac (cy - cy') / 180 * pi * sensitivity
            pitch %= min 1 . max (-1)
          front <- do
            p <- use pitch
            y <- use yaw
            pure $ normalize $
              V3
                (cos y * cos p)
                (sin p)
                (sin y * cos p)
          let right = cross front (unit _y)
          let up = cross right front
          let camSpeed = cameraSpeed * dt
          whenM (isKeyPressed Key'W) $ pos += camSpeed *^ front
          whenM (isKeyPressed Key'S) $ pos -= camSpeed *^ front
          whenM (isKeyPressed Key'A) $ pos -= camSpeed *^ normalize (cross front up)
          whenM (isKeyPressed Key'D) $ pos += camSpeed *^ normalize (cross front up)
          whenM (isKeyPressed Key'Space) $ pos += camSpeed *^ up
          whenM (isKeyPressed Key'LeftControl) $ pos -= camSpeed *^ up
          --
          x <- use pos
          let view = lookAt x (x + front) up
          --
          useProgram cubeProg
          setUniform cubeViewPos x
          setUniform cubeView view
          setUniform cubeModel identity
          forM_ meshes drawMesh
          --
          fmap or . mapM isKeyPressed $ [Key'Escape, Key'Q]
-- TODO - use shouldClose
-- return quit
