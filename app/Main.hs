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
import NonGL
import Types
import Window

{-# ANN module "hlint: ignore Redundant bracket" #-}

data AppEnv
  = AppEnv
      { cubeVAO :: VAO,
        cubeView :: Uniform (M44 Float),
        cubeModel :: Uniform (M44 Float),
        cubeProj :: Uniform (M44 Float),
        cubeLightCol :: Uniform (V3 Float),
        cubeLightPos :: Uniform (V3 Float),
        cubeViewPos :: Uniform (V3 Float),
        cubeSelfCol :: Uniform (V3 Float),
        cubeProg :: Program,
        lightVAO :: VAO,
        lightView :: Uniform (M44 Float),
        lightModel :: Uniform (M44 Float),
        lightProj :: Uniform (M44 Float),
        lightCol :: Uniform (V3 Float),
        lightProg :: Program
      }

data AppState
  = AppState
      { _pos :: V3 Float,
        _pitch :: Float,
        _yaw :: Float,
        _cursorPrev :: (Double, Double)
      }

makeLenses ''AppState

verticesNormals :: [(V3 Float, V3 Float)]
verticesNormals =
  [ (V3 (-0.5) (-0.5) (-0.5), V3 0 0 (-1)),
    (V3 (0.5) (-0.5) (-0.5), V3 0 0 (-1)),
    (V3 (0.5) (0.5) (-0.5), V3 0 0 (-1)),
    (V3 (0.5) (0.5) (-0.5), V3 0 0 (-1)),
    (V3 (-0.5) (0.5) (-0.5), V3 0 0 (-1)),
    (V3 (-0.5) (-0.5) (-0.5), V3 0 0 (-1)),
    --
    (V3 (-0.5) (-0.5) (0.5), V3 0 0 1),
    (V3 (0.5) (-0.5) (0.5), V3 0 0 1),
    (V3 (0.5) (0.5) (0.5), V3 0 0 1),
    (V3 (0.5) (0.5) (0.5), V3 0 0 1),
    (V3 (-0.5) (0.5) (0.5), V3 0 0 1),
    (V3 (-0.5) (-0.5) (0.5), V3 0 0 1),
    --
    (V3 (-0.5) (0.5) (0.5), V3 (-1) 0 0),
    (V3 (-0.5) (0.5) (-0.5), V3 (-1) 0 0),
    (V3 (-0.5) (-0.5) (-0.5), V3 (-1) 0 0),
    (V3 (-0.5) (-0.5) (-0.5), V3 (-1) 0 0),
    (V3 (-0.5) (-0.5) (0.5), V3 (-1) 0 0),
    (V3 (-0.5) (0.5) (0.5), V3 (-1) 0 0),
    --
    (V3 (0.5) (0.5) (0.5), V3 1 0 0),
    (V3 (0.5) (0.5) (-0.5), V3 1 0 0),
    (V3 (0.5) (-0.5) (-0.5), V3 1 0 0),
    (V3 (0.5) (-0.5) (-0.5), V3 1 0 0),
    (V3 (0.5) (-0.5) (0.5), V3 1 0 0),
    (V3 (0.5) (0.5) (0.5), V3 1 0 0),
    --
    (V3 (-0.5) (-0.5) (-0.5), V3 0 (-1) 0),
    (V3 (0.5) (-0.5) (-0.5), V3 0 (-1) 0),
    (V3 (0.5) (-0.5) (0.5), V3 0 (-1) 0),
    (V3 (0.5) (-0.5) (0.5), V3 0 (-1) 0),
    (V3 (-0.5) (-0.5) (0.5), V3 0 (-1) 0),
    (V3 (-0.5) (-0.5) (-0.5), V3 0 (-1) 0),
    --
    (V3 (-0.5) (0.5) (-0.5), V3 0 1 0),
    (V3 (0.5) (0.5) (-0.5), V3 0 1 0),
    (V3 (0.5) (0.5) (0.5), V3 0 1 0),
    (V3 (0.5) (0.5) (0.5), V3 0 1 0),
    (V3 (-0.5) (0.5) (0.5), V3 0 1 0),
    (V3 (-0.5) (0.5) (-0.5), V3 0 1 0)
  ]

projectionM :: M44 Float
projectionM = perspective (45 / 180 * pi) (640 / 480) 0.1 100

cameraSpeed :: Float
cameraSpeed = 2.5

main :: IO ()
main = withWindow defaultHints $ do
  hideCursor
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
      cubeVAO <- genArray
      bindArray cubeVAO
      vbo <- genBuffer
      bufferData vbo verticesNormals GL_ARRAY_BUFFER GL_STATIC_DRAW -- also binds
      setVertexAttribs verticesNormals
      unbindArray
      --
      glEnable GL_DEPTH_TEST
      --
      lightVAO <- genArray
      bindArray lightVAO
      glBindBuffer GL_ARRAY_BUFFER (unBuffer vbo)
      setVertexAttribs verticesNormals
      unbindArray
      cubeProg <-
        createShaderProgram
          [ ("glsl/common.vert", GL_VERTEX_SHADER),
            ("glsl/object.frag", GL_FRAGMENT_SHADER)
          ]
      lightProg <-
        createShaderProgram
          [ ("glsl/common.vert", GL_VERTEX_SHADER),
            ("glsl/light.frag", GL_FRAGMENT_SHADER)
          ]
      cubeView <- getUniform cubeProg "view"
      cubeModel <- getUniform cubeProg "model"
      cubeProj <- getUniform cubeProg "projection"
      cubeLightCol <- getUniform cubeProg "lightColor"
      cubeLightPos <- getUniform cubeProg "lightPos"
      cubeViewPos <- getUniform cubeProg "viewPos"
      cubeSelfCol <- getUniform cubeProg "objectColor"
      lightView <- getUniform lightProg "view"
      lightModel <- getUniform lightProg "model"
      lightProj <- getUniform lightProg "projection"
      lightCol <- getUniform lightProg "lightColor"
      pure AppEnv {..}
    loop (AppEnv {..}) sInit =
      flip runStateT sInit $ do
        pos .= V3 0 0 6
        -- front .= -unit _z
        -- up    .= unit _y
        yaw .= (-90 / 180 * pi)
        bufferSwapLoop $ \dt -> do
          () <- do
            let sensitivity = 0.1
            (cx', cy') <- use cursorPrev
            c@(cx, cy) <- getCursorPos
            cursorPrev .= c
            yaw += realToFrac (cx - cx') / 180 * pi * sensitivity
            pitch -= realToFrac (cy - cy') / 180 * pi * sensitivity
            pitch %= min 1 . max (-1)
          -- pitch %= const 0
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
          -- x <- use pos
          -- liftIO $ do
          --   putStrLn $ "front: " <> show (round <$> front)
          --   putStrLn $ "pos:   " <> show (round <$> x)
          --   putStrLn $ "up:    " <> show (round <$> up)
          let camSpeed = cameraSpeed * dt
          whenM (isKeyPressed Key'W) $ pos += camSpeed *^ front
          whenM (isKeyPressed Key'S) $ pos -= camSpeed *^ front
          whenM (isKeyPressed Key'A) $ pos -= camSpeed *^ normalize (cross front up)
          whenM (isKeyPressed Key'D) $ pos += camSpeed *^ normalize (cross front up)
          whenM (isKeyPressed Key'Space) $ pos += camSpeed *^ up
          whenM (isKeyPressed Key'LeftControl) $ pos -= camSpeed *^ up
          --
          time <- getTime
          x <- use pos
          let view = lookAt x (x + front) up
          let lightPos = V3 1.2 1 2
          --
          useProgram cubeProg
          setUniform cubeSelfCol (V3 1 0.5 0.31)
          setUniform cubeLightCol 1
          setUniform cubeLightPos lightPos
          setUniform cubeModel $ mkTransformation (axisAngle (V3 0.5 1 0) (time / 9 * pi)) 0
          setUniform cubeViewPos x
          setUniform cubeView view
          setUniform cubeProj projectionM
          bindArray cubeVAO
          glDrawArrays GL_TRIANGLES 0 36
          unbindArray
          --
          useProgram lightProg
          setUniform lightCol 1
          setUniform lightModel $
            identity
              & translation .~ lightPos
              & _m33 %~ (* 0.2)
          setUniform lightView view
          setUniform lightProj projectionM
          bindArray lightVAO
          glDrawArrays GL_TRIANGLES 0 36
          -- glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_INT nullPtr
          --
          unbindArray
          quit <- fmap or . mapM isKeyPressed $ [Key'Escape, Key'Q]
          return quit
