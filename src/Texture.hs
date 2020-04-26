module Texture where

import Codec.Picture as Pic hiding (Uniform)
import Control.Monad.Except
import Data.Vector.Storable (unsafeWith)
import Foreign as F
import GLStorable
import Graphics.GL.Core33 as GL
import Graphics.GL.Types as GL

data TextureError
  = ImageNotFound FilePath

newtype Texture = Texture {unTexture :: GLuint}

ppTextureError :: TextureError -> String
ppTextureError (ImageNotFound path) = "Image not found: " <> path

-- Warning; Doesn't just allocate a texture and load data into it;
-- uploading to GPU also necessarily binds texture to active texture unit
loadTextureRGB2D ::
  MonadIO m =>
  FilePath ->
  Bool ->
  Bool ->
  ExceptT TextureError m Texture
loadTextureRGB2D path flipX flipY = do
  img <- withExceptT ImageNotFound . ExceptT . liftIO $ readImage path
  let (Image imgW imgH imgData) = flipImg flipX flipY . convertRGB8 $ img
  tex <- liftIO $ alloca $ \tex' -> do
    glGenTextures 1 tex'
    peek tex'
  glBindTexture GL_TEXTURE_2D tex
  liftIO $ unsafeWith imgData $ \imgPtr ->
    glTexImage2D
      GL_TEXTURE_2D
      0
      GL_RGB
      (fromIntegral imgW)
      (fromIntegral imgH)
      0
      GL_RGB
      GL_UNSIGNED_BYTE
      (castPtr imgPtr)
  glGenerateMipmap GL_TEXTURE_2D
  mapM_
    (uncurry $ glTexParameteri GL_TEXTURE_2D)
    [ (GL_TEXTURE_WRAP_S, GL_REPEAT),
      (GL_TEXTURE_WRAP_T, GL_REPEAT),
      (GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR),
      (GL_TEXTURE_MAG_FILTER, GL_LINEAR)
    ]
  pure $ Texture tex

flipImg :: Pixel a => Bool -> Bool -> Pic.Image a -> Pic.Image a
flipImg flipX flipY img = generateImage f (imageWidth img) (imageHeight img)
  where
    w' = imageWidth img - 1
    h' = imageHeight img - 1
    f x y = pixelAt img (if flipX then w' - x else x) (if flipY then h' - y else y)

bindTexture :: MonadIO m => Texture -> m ()
bindTexture (Texture t) = glBindTexture GL_TEXTURE_2D t
