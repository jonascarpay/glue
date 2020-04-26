{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Mesh
  ( Mesh (..),
    setupMesh,
    GPUMesh (..),
    drawMesh,
    triangle,
    fromVertexList,
    toVertexList,
  )
where

import Control.Monad.IO.Class
import Data.Foldable (foldl')
import qualified Data.Map as M
import qualified Data.Vector.Storable as VS
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import GLStorable
import Graphics.GL.Core33
import Graphics.GL.Types
import Lib
import Linear
import Program
import Types
import Window

data Mesh v
  = Mesh
      { vertices :: VS.Vector v,
        indices :: VS.Vector Word32
      }
  deriving (Eq, Show)

fromVertexList :: forall v. (Storable v, Ord v) => [v] -> Mesh v
fromVertexList vs = Mesh (VS.fromList $ reverse stack) (VS.fromList $ (imap M.!) <$> vs)
  where
    (imap, _, stack) :: (M.Map v Word32, Word32, [v]) =
      let f (!imap, !n, !stack) v
            | M.member v imap = (imap, n, stack)
            | otherwise = (M.insert v n imap, succ n, v : stack)
       in foldl' f (mempty, 0, mempty) vs

toVertexList :: VS.Storable v => Mesh v -> [v]
toVertexList (Mesh vs is) = map (vs VS.!) . fmap fromIntegral . VS.toList $ is

setupMesh :: forall m v. (GLVertex v, MonadWindow m) => Mesh v -> m (GPUMesh v)
setupMesh (Mesh v i) = do
  vao <- genArray
  Buffer vbo <- genBuffer
  Buffer ebo <- genBuffer
  bindArray vao
  glBindBuffer GL_ARRAY_BUFFER vbo
  liftIO $ VS.unsafeWith v $ \ptr ->
    let bytes = VS.length v * sizeOf (undefined :: v)
     in glBufferData GL_ARRAY_BUFFER (fromIntegral bytes) (castPtr ptr) GL_STATIC_DRAW
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo
  liftIO $ VS.unsafeWith i $ \ptr ->
    let bytes = VS.length i * sizeOf (undefined :: Word32)
     in glBufferData GL_ELEMENT_ARRAY_BUFFER (fromIntegral bytes) (castPtr ptr) GL_STATIC_DRAW
  setVertexAttribs v
  unbindArray
  pure $ GPUMesh vao (Buffer vbo) (Buffer ebo) (fromIntegral $ 3 * VS.length i)

triangle :: Mesh (V3 Float)
triangle =
  Mesh
    (VS.fromList [V3 (-0.5) (-0.5) 0, V3 0.5 (-0.5) 0, V3 0 0.5 0])
    (VS.fromList [0, 1, 2])

data GPUMesh v
  = GPUMesh
      { meshVAO :: VAO,
        meshVBO :: Buffer,
        meshIndexBuffer :: Buffer,
        meshIndexCount :: GLsizei
      }

drawMesh :: MonadWindow m => GPUMesh v -> ProgramT mat v m ()
drawMesh (GPUMesh vao _ _ n) = ProgramT $ do
  bindArray vao
  glDrawElements GL_TRIANGLES n GL_UNSIGNED_INT nullPtr
  unbindArray
