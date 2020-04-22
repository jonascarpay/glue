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
        cubeViewPos :: Uniform (V3 Float),
        cubeProg :: Program,
        lightVAO :: VAO,
        lightView :: Uniform (M44 Float),
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

verticesNormals :: [(V3 Float, V3 Float, V2 Float)]
verticesNormals =
  [ (V3 (-0.5) (-0.5) (-0.5), V3 0 0 (-1), V2 0 0),
    (V3 (0.5) (-0.5) (-0.5), V3 0 0 (-1), V2 1 0),
    (V3 (0.5) (0.5) (-0.5), V3 0 0 (-1), V2 1 1),
    (V3 (0.5) (0.5) (-0.5), V3 0 0 (-1), V2 1 1),
    (V3 (-0.5) (0.5) (-0.5), V3 0 0 (-1), V2 0 1),
    (V3 (-0.5) (-0.5) (-0.5), V3 0 0 (-1), V2 0 0),
    --
    (V3 (-0.5) (-0.5) (0.5), V3 0 0 1, V2 0 0),
    (V3 (0.5) (-0.5) (0.5), V3 0 0 1, V2 1 0),
    (V3 (0.5) (0.5) (0.5), V3 0 0 1, V2 1 1),
    (V3 (0.5) (0.5) (0.5), V3 0 0 1, V2 1 1),
    (V3 (-0.5) (0.5) (0.5), V3 0 0 1, V2 0 1),
    (V3 (-0.5) (-0.5) (0.5), V3 0 0 1, V2 0 0),
    --
    (V3 (-0.5) (0.5) (0.5), V3 (-1) 0 0, V2 1 0),
    (V3 (-0.5) (0.5) (-0.5), V3 (-1) 0 0, V2 1 1),
    (V3 (-0.5) (-0.5) (-0.5), V3 (-1) 0 0, V2 0 1),
    (V3 (-0.5) (-0.5) (-0.5), V3 (-1) 0 0, V2 0 1),
    (V3 (-0.5) (-0.5) (0.5), V3 (-1) 0 0, V2 0 0),
    (V3 (-0.5) (0.5) (0.5), V3 (-1) 0 0, V2 1 0),
    --
    (V3 (0.5) (0.5) (0.5), V3 1 0 0, V2 1 0),
    (V3 (0.5) (0.5) (-0.5), V3 1 0 0, V2 1 1),
    (V3 (0.5) (-0.5) (-0.5), V3 1 0 0, V2 0 1),
    (V3 (0.5) (-0.5) (-0.5), V3 1 0 0, V2 0 1),
    (V3 (0.5) (-0.5) (0.5), V3 1 0 0, V2 0 0),
    (V3 (0.5) (0.5) (0.5), V3 1 0 0, V2 1 0),
    --
    (V3 (-0.5) (-0.5) (-0.5), V3 0 (-1) 0, V2 0 1),
    (V3 (0.5) (-0.5) (-0.5), V3 0 (-1) 0, V2 1 1),
    (V3 (0.5) (-0.5) (0.5), V3 0 (-1) 0, V2 1 0),
    (V3 (0.5) (-0.5) (0.5), V3 0 (-1) 0, V2 1 0),
    (V3 (-0.5) (-0.5) (0.5), V3 0 (-1) 0, V2 0 0),
    (V3 (-0.5) (-0.5) (-0.5), V3 0 (-1) 0, V2 0 1),
    --
    (V3 (-0.5) (0.5) (-0.5), V3 0 1 0, V2 0 1),
    (V3 (0.5) (0.5) (-0.5), V3 0 1 0, V2 1 1),
    (V3 (0.5) (0.5) (0.5), V3 0 1 0, V2 1 0),
    (V3 (0.5) (0.5) (0.5), V3 0 1 0, V2 1 0),
    (V3 (-0.5) (0.5) (0.5), V3 0 1 0, V2 0 0),
    (V3 (-0.5) (0.5) (-0.5), V3 0 1 0, V2 0 1)
  ]

positions :: [V3 Float]
positions =
  [ V3 0 0 0,
    V3 (2.0) (5.0) (-15.0),
    V3 (-1.5) (-2.2) (-2.5),
    V3 (-3.8) (-2.0) (-12.3),
    V3 (2.4) (-0.4) (-3.5),
    V3 (-1.7) (3.0) (-7.5),
    V3 (1.3) (-2.0) (-2.5),
    V3 (1.5) (2.0) (-2.5),
    V3 (1.5) (0.2) (-1.5),
    V3 (-1.3) (1.0) (-1.5)
  ]

lightPos :: V3 Float
lightPos = sum positions / fromIntegral (length positions)

lightModel :: M44 Float
lightModel = identity & translation .~ lightPos & _m33 %~ (* 0.2)

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
      do
        -- No need to pass these to the program, textures stay bound
        glActiveTexture GL_TEXTURE0
        loadTextureRGB2D "assets/container_diffuse.png" False False
        glActiveTexture GL_TEXTURE1
        loadTextureRGB2D "assets/container_specular.png" False False
      --
      glEnable GL_DEPTH_TEST
      --
      cubeVAO <- genArray
      bindArray cubeVAO
      vbo <- genBuffer
      bufferData vbo verticesNormals GL_ARRAY_BUFFER GL_STATIC_DRAW -- also binds
      setVertexAttribs verticesNormals
      unbindArray
      --
      lightVAO <- genArray
      bindArray lightVAO
      glBindBuffer GL_ARRAY_BUFFER (unBuffer vbo)
      setVertexAttribs verticesNormals
      unbindArray
      --
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
      do
        useProgram lightProg
        setUniformByName lightProg "lightColor" (1 :: V3 Float)
        setUniformByName lightProg "model" lightModel
        setUniformByName lightProg "projection" projectionM
      do
        useProgram cubeProg -- Can only set after binding
        setUniformByName cubeProg "material.shininess" (32 :: Float)
        setUniformByName cubeProg "material.diffuse" (0 :: TextureUnit)
        setUniformByName cubeProg "material.specular" (1 :: TextureUnit)
        setUniformByName cubeProg "projection" projectionM
        setUniformByName cubeProg "light.ambient" (0.1 :: V3 Float)
        setUniformByName cubeProg "light.diffuse" (1 :: V3 Float)
        setUniformByName cubeProg "light.specular" (1 :: V3 Float)
        -- setUniformByName cubeProg "light.direction" (- unit _y :: V3 Float)
        setUniformByName cubeProg "light.position" lightPos
      cubeModel <- getUniform cubeProg "model"
      cubeView <- getUniform cubeProg "view"
      cubeViewPos <- getUniform cubeProg "viewPos"
      lightView <- getUniform lightProg "view"
      pure AppEnv {..}
    loop (AppEnv {..}) sInit =
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
          --
          useProgram cubeProg
          -- setUniform cubeModel $ mkTransformation (axisAngle (V3 0.5 1 0) (time / 3 * pi)) 0
          setUniform cubeViewPos x
          setUniform cubeView view
          bindArray cubeVAO
          forM_ positions $ \cubePos -> do
            setUniform cubeModel $ mkTransformation (axisAngle (V3 0.5 1 0) (time / 3 * pi)) cubePos
            glDrawArrays GL_TRIANGLES 0 36
          unbindArray
          --
          useProgram lightProg
          setUniform lightView view
          bindArray lightVAO
          glDrawArrays GL_TRIANGLES 0 36
          unbindArray
          --
          fmap or . mapM isKeyPressed $ [Key'Escape, Key'Q]
-- TODO - use shouldClose
-- return quit
