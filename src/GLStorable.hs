{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module GLStorable where

import Control.Monad.IO.Class
import Foreign as F
import Foreign.Storable.Tuple ()
import Graphics.GL.Core33 as GL
import Graphics.GL.Types as GL
import Linear
import Types

class GLUniform a where
  setUniform :: MonadIO m => Uniform a -> a -> m ()

instance GLUniform (M44 Float) where
  setUniform (Uniform ptr) x =
    liftIO $ F.with x $ glUniformMatrix4fv ptr 1 GL_TRUE . castPtr

instance GLUniform (V3 Float) where
  setUniform (Uniform ptr) (V3 a b c) = glUniform3f ptr a b c

instance GLUniform Float where
  setUniform (Uniform ptr) = glUniform1f ptr

instance GLUniform TextureUnit where
  setUniform (Uniform ptr) (TextureUnit slot) = glUniform1i ptr slot

data VertexAttribs
  = VertexAttribs
      { attrSize :: GLint,
        attrType :: GLenum,
        attrNormalize :: GLboolean
      }

class Storable a => GLVertex a where vertexAttribs :: p a -> [VertexAttribs]

instance GLVertex Float where vertexAttribs _ = [VertexAttribs 1 GL_FLOAT GL_FALSE]

instance GLVertex (V1 Float) where vertexAttribs _ = [VertexAttribs 1 GL_FLOAT GL_FALSE]

instance GLVertex (V2 Float) where vertexAttribs _ = [VertexAttribs 2 GL_FLOAT GL_FALSE]

instance GLVertex (V3 Float) where vertexAttribs _ = [VertexAttribs 3 GL_FLOAT GL_FALSE]

instance GLVertex (V4 Float) where vertexAttribs _ = [VertexAttribs 4 GL_FLOAT GL_FALSE]

instance GLVertex Word32 where vertexAttribs _ = [VertexAttribs 1 GL_INT GL_FALSE]

instance GLVertex (V1 Word32) where vertexAttribs _ = [VertexAttribs 1 GL_INT GL_FALSE]

instance GLVertex (V2 Word32) where vertexAttribs _ = [VertexAttribs 1 GL_INT GL_FALSE]

instance GLVertex (V3 Word32) where vertexAttribs _ = [VertexAttribs 1 GL_INT GL_FALSE]

instance GLVertex (V4 Word32) where vertexAttribs _ = [VertexAttribs 1 GL_INT GL_FALSE]

instance (GLVertex a, GLVertex b) => GLVertex (a, b) where
  vertexAttribs _ = vertexAttribs (undefined :: p a) <> vertexAttribs (undefined :: p b)

instance (GLVertex a, GLVertex b, GLVertex c) => GLVertex (a, b, c) where
  vertexAttribs _ =
    vertexAttribs (undefined :: p a)
      <> vertexAttribs (undefined :: p b)
      <> vertexAttribs (undefined :: p c)

instance (GLVertex a, GLVertex b, GLVertex c, GLVertex d) => GLVertex (a, b, c, d) where
  vertexAttribs _ =
    vertexAttribs (undefined :: p a)
      <> vertexAttribs (undefined :: p b)
      <> vertexAttribs (undefined :: p c)
      <> vertexAttribs (undefined :: p d)
