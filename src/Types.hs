module Types where

import Graphics.GL.Types  as GL

newtype Uniform a = Uniform {unUniform :: GLint}
newtype Program = Program {unProgram :: GLuint}
newtype Texture = Texture {unTexture :: GLuint}
newtype VAO = VAO {unVAO :: GLuint}
newtype Buffer = Buffer {unBuffer :: GLuint}
