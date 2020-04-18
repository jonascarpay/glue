{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module GLStorable where

import Control.Monad.IO.Class
import Foreign                as F

import Graphics.GL.Core33 as GL
import Graphics.GL.Types  as GL

import Linear

import Types

class GLUniform a where
  setUniform :: MonadIO m => Uniform a -> a -> m ()

instance GLUniform (M44 Float) where
  setUniform (Uniform ptr) val =
    liftIO $ with val $ glUniformMatrix4fv ptr 1 GL_TRUE . castPtr

instance GLUniform GLint where setUniform (Uniform ptr) = glUniform1i ptr

data a :. as where
  (:.) :: a -> as -> (a :. as)

infixr 5 :.

instance (Storable a, Storable as) => Storable (a :. as) where
  {-# INLINE sizeOf #-}
  sizeOf    _ = sizeOf    (undefined :: a)   +   sizeOf    (undefined :: as)
  {-# INLINE alignment #-}
  alignment _ = alignment (undefined :: a) `lcm` alignment (undefined :: as)
  {-# INLINE peek #-}
  peek ptr = do
    let ptr' = alignPtr ptr (alignment (undefined :: a))
    a  <- peek (castPtr ptr')
    as <- peek . castPtr $ plusPtr ptr' (sizeOf (undefined :: a))
    pure $ a :. as
  {-# INLINE poke #-}
  poke ptr (a :. as) = do
    poke (castPtr ptr) a
    poke (castPtr $ plusPtr ptr (sizeOf a)) as

class Storable a => GLAttrib a where
  attribs :: a -> [(GLint, Word32, GLboolean)]

instance GLAttrib Float      where attribs _ = [(1, GL_FLOAT, GL_FALSE)]
instance GLAttrib (V1 Float) where attribs _ = [(1, GL_FLOAT, GL_FALSE)]
instance GLAttrib (V2 Float) where attribs _ = [(2, GL_FLOAT, GL_FALSE)]
instance GLAttrib (V3 Float) where attribs _ = [(3, GL_FLOAT, GL_FALSE)]
instance GLAttrib (V4 Float) where attribs _ = [(4, GL_FLOAT, GL_FALSE)]

instance GLAttrib Word32      where attribs _ = [(1, GL_INT, GL_FALSE)]
instance GLAttrib (V1 Word32) where attribs _ = [(1, GL_INT, GL_FALSE)]
instance GLAttrib (V2 Word32) where attribs _ = [(2, GL_INT, GL_FALSE)]
instance GLAttrib (V3 Word32) where attribs _ = [(3, GL_INT, GL_FALSE)]
instance GLAttrib (V4 Word32) where attribs _ = [(4, GL_INT, GL_FALSE)]

instance (GLAttrib a, GLAttrib b) => GLAttrib (a :. b) where attribs _ = attribs (undefined :: a) <> attribs (undefined :: b)
