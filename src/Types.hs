{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Graphics.GL.Types as GL

newtype Texture = Texture {unTexture :: GLuint}

newtype TextureUnit = TextureUnit {unTextureUnit :: GLint}
  deriving (Num)

newtype VAO = VAO {unVAO :: GLuint}

newtype Buffer = Buffer {unBuffer :: GLuint}
