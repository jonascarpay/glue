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
import Data.Int (Int32)
import Graphics.GL.Core33 as GL
import Graphics.UI.GLFW as GLFW hiding (getCursorPos, getTime)
import Linear
import Mesh
import NonGL
import Obj
import Program
import Texture
import Window

data AppState
  = AppState
      { _pos :: V3 Float,
        _pitch :: Float,
        _yaw :: Float,
        _cursorPrev :: (Double, Double)
      }

makeLenses ''AppState

type ObjVert = (V3 Float, V3 Float, V2 Float)

data AppEnv
  = AppEnv
      { program :: Program ObjectMat ObjVert,
        meshes :: [GPUMesh ObjVert]
      }

lightPos :: V3 Float
lightPos = V3 5 3 0

projectionM :: M44 Float
projectionM = perspective (45 / 180 * pi) (640 / 480) 0.1 100

data ObjectMat f
  = ObjectMat
      { objView :: f (M44 Float),
        objModel :: f (M44 Float),
        objProj :: f (M44 Float),
        objViewPos :: f (V3 Float),
        objLightAmbient :: f (V3 Float),
        objLightDiffuse :: f (V3 Float),
        objLightSpecular :: f (V3 Float),
        objLightPosition :: f (V3 Float),
        objDiffuseMap :: f Int32,
        objSpecularMap :: f Int32,
        objShininess :: f Float
      }

objInit :: MaterialInitializer ObjectMat
objInit f =
  ObjectMat
    <$> f objView 0 "view"
    <*> f objModel identity "model"
    <*> f objProj projectionM "projection"
    <*> f objViewPos 0 "viewPos"
    <*> f objLightAmbient 0.2 "light.ambient"
    <*> f objLightDiffuse 1 "light.diffuse"
    <*> f objLightSpecular 1 "light.specular"
    <*> f objLightPosition lightPos "light.position"
    <*> f objDiffuseMap 0 "material.diffuse"
    <*> f objSpecularMap 1 "material.specular"
    <*> f objShininess 32 "material.shininess"

cameraSpeed :: Float
cameraSpeed = 10

main :: IO ()
main = withWindow defaultHints $ do
  hideCursor
  menv <- runExceptT buildEnvironment
  case menv of
    Left err -> liftIO . putStrLn $ err
    Right env -> do
      p <- getCursorPos
      loop env (AppState 0 0 0 p)
      pure ()
  where
    buildEnvironment :: ExceptT String (WindowT IO) AppEnv
    buildEnvironment = do
      --
      glEnable GL_DEPTH_TEST
      --
      program <- withExceptT ppProgramError $ do
        (program, log) <- createProgram "glsl/common.vert" "glsl/object.frag" objInit
        forM_ log $ liftIO . putStrLn . ppProgramLog
        pure program
      -- meshes <-
      --   liftIO (loadObj "/home/jmc/Downloads/nanosuit/nanosuit.obj")
      --     >>= either throwError pure
      --     >>= traverse setupMesh . toMesh
      meshes <- pure <$> setupMesh cube
      withExceptT ppTextureError $ do
        glActiveTexture GL_TEXTURE0
        loadTextureRGB2D "assets/container_diffuse.png" False False
        glActiveTexture GL_TEXTURE1
        loadTextureRGB2D "assets/container_specular.png" False False
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
          runProgramT program $ do
            setUniform objViewPos x
            setUniform objView view
            setUniform objModel identity
            forM_ meshes drawMesh
          --
          fmap or . mapM isKeyPressed $ [Key'Escape, Key'Q]
-- TODO - use shouldClose
-- return quit
