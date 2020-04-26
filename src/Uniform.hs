{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Uniform where

import Control.Monad.Except
import Data.Functor ((<&>))
import Foreign as F
import Foreign.C.String as F
import Graphics.GL.Core33
import Graphics.GL.Types
import Linear

class Uniform a where
  setUniformRaw :: MonadIO m => GLint -> a -> m ()

instance Uniform (M44 Float) where
  setUniformRaw uid x =
    liftIO $ F.with x $ glUniformMatrix4fv uid 1 GL_TRUE . castPtr

instance Uniform (V3 Float) where
  setUniformRaw uid (V3 a b c) = glUniform3f uid a b c

instance Uniform Float where setUniformRaw = glUniform1f

instance Uniform Int32 where setUniformRaw = glUniform1i

-- instance Uniform TextureUnit where
--   setUniformRaw uid (TextureUnit slot) = glUniform1i uid slot

newtype UniformNotFoundError = UniformNotFoundError String

getUniformLocation :: MonadIO m => String -> GLuint -> m (Maybe GLint)
getUniformLocation name program =
  liftIO (F.withCString name $ glGetUniformLocation program) <&> \case
    -1 -> Nothing
    n -> (Just n)
