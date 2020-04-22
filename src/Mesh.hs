{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mesh
  ( Mesh,
  )
where

import Control.Monad.IO.Class
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Data.Word (Word32)
import Foreign.Ptr
import Foreign.Storable
import GLStorable
import Graphics.GL.Core33
import Lib
import Types
import Window

data Mesh v
  = Mesh
      { vertices :: VS.Vector v,
        indices :: VS.Vector Word32,
        textures :: V.Vector Texture'
      }
  deriving (Eq, Show)

data Texture'
  = Texture'
      { texID :: Int,
        texType :: TextureType
      }
  deriving (Eq, Show)

data TextureType = Diffuse | Specular | Bump
  deriving (Eq, Show)

setupMesh :: forall m v. (GLVertex v, MonadWindow m) => Mesh v -> m ()
setupMesh (Mesh v i t) = do
  vao <- genArray
  Buffer vbo <- genBuffer
  Buffer ebo <- genBuffer
  bindArray vao
  glBindBuffer GL_ARRAY_BUFFER vbo
  liftIO $ VS.unsafeWith v $ \ptr ->
    let bytes = VS.length v * sizeOf (undefined :: v)
     in glBufferData GL_ARRAY_BUFFER (fromIntegral bytes) (castPtr ptr) GL_STATIC_DRAW
  glBindBuffer GL_ARRAY_BUFFER ebo
  liftIO $ VS.unsafeWith i $ \ptr ->
    let bytes = VS.length i * sizeOf (undefined :: Word32)
     in glBufferData GL_ARRAY_BUFFER (fromIntegral bytes) (castPtr ptr) GL_STATIC_DRAW
  setVertexAttribs v
