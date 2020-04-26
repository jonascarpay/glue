{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Window where

import Control.Monad.Except
import Control.Monad.Fail
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe (fromJust)
import Foreign as F
import Graphics.GL.Core33 as GL
import Graphics.UI.GLFW as GLFW

newtype WindowT m a = WindowT (ReaderT Window m a)
  deriving (Functor, Monad, MonadTrans, Applicative, MonadIO, MonadFail)

class MonadIO m => MonadWindow m where
  askWindow :: m Window

instance MonadIO m => MonadWindow (WindowT m) where
  {-# INLINE askWindow #-}
  askWindow = WindowT ask

instance MonadWindow m => MonadWindow (StateT s m) where
  {-# INLINE askWindow #-}
  askWindow = lift askWindow

instance MonadWindow m => MonadWindow (ReaderT r m) where
  {-# INLINE askWindow #-}
  askWindow = lift askWindow

instance MonadWindow m => MonadWindow (ExceptT e m) where
  {-# INLINE askWindow #-}
  askWindow = lift askWindow

instance (Monoid w, MonadWindow m) => MonadWindow (WriterT w m) where
  {-# INLINE askWindow #-}
  askWindow = lift askWindow

withWindow :: [WindowHint] -> WindowT IO () -> IO ()
withWindow hints (WindowT act) = do
  True <- GLFW.init
  mapM_ GLFW.windowHint hints
  -- mon <- GLFW.getPrimaryMonitor
  mwin <- GLFW.createWindow 800 600 "TITEL" Nothing Nothing
  forM_ mwin $ \win -> do
    GLFW.makeContextCurrent (Just win)
    GL.glViewport 0 0 800 600
    GLFW.setFramebufferSizeCallback win (Just fbSizeCB)
    runReaderT act win
  GLFW.terminate

defaultHints :: [WindowHint]
defaultHints =
  [ WindowHint'ContextVersionMajor 4,
    WindowHint'ContextVersionMinor 3,
    WindowHint'OpenGLProfile OpenGLProfile'Core,
    WindowHint'OpenGLForwardCompat True,
    WindowHint'Floating True,
    WindowHint'Resizable False
  ]

hideCursor :: MonadWindow m => m ()
hideCursor = do
  win <- askWindow
  liftIO $ GLFW.setCursorInputMode win CursorInputMode'Disabled

getCursorPos :: MonadWindow m => m (Double, Double)
getCursorPos = askWindow >>= \win -> liftIO $ GLFW.getCursorPos win

fbSizeCB :: Window -> Int -> Int -> IO ()
fbSizeCB _win hres vres =
  putStrLn $ "Framebuffer resized to " <> show hres <> "x" <> show vres

bufferSwapLoop :: MonadWindow m => (Float -> m Bool) -> m ()
bufferSwapLoop act = do
  t0 <- getTime'
  go t0
  where
    getTime' = liftIO $ realToFrac . fromJust <$> getTime
    go tPrev = do
      win <- askWindow
      glShouldClose <- liftIO $ GLFW.windowShouldClose win -- FIXME this is outside the loop
      unless glShouldClose $ do
        liftIO $ GLFW.swapBuffers win
        liftIO GLFW.pollEvents
        glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT
        tNow <- getTime'
        appShouldClose <- act (tNow - tPrev)
        unless appShouldClose $ go tNow

isKeyPressed :: MonadWindow m => GLFW.Key -> m Bool
isKeyPressed key = do
  win <- askWindow
  (== KeyState'Pressed) <$> liftIO (GLFW.getKey win key)
