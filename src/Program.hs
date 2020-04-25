{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Program
  ( Program,
    createProgram,
    MaterialInitializer,
    ProgramT (..),
    runProgramT,
    setUniform,
    setMaterial,
    ProgramError (..),
    ppProgramError,
    ProgramLog,
    ppProgramLog,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Functor (($>))
import Data.Functor.Identity
import Data.Proxy
import qualified Foreign as F
import qualified Foreign.C.String as F
import Graphics.GL.Core33
import Graphics.GL.Types
import Uniform

data Program (m :: (* -> *) -> *) v
  = Program
      { programId :: !GLuint,
        _setUnis :: !(m Setter),
        _setMat :: !(Setter (m Identity))
      }

newtype Setter a = Setter {runSetter :: a -> IO ()}

newtype ProgramT mat vert m a = ProgramT {unProgramT :: ReaderT (Program mat vert) m a}
  deriving (Functor, Monad, Applicative)

runProgramT :: MonadIO m => Program mat vert -> ProgramT mat vert m a -> m a
runProgramT p (ProgramT m) = do
  glUseProgram (programId p)
  runReaderT m p

type MaterialInitializer mat =
  forall m f.
  Applicative m =>
  (forall a. Uniform a => (forall g. mat g -> g a) -> a -> String -> m (f a)) ->
  m (mat f)

data ProgramError
  = UniformNotFound String
  | ShaderObjectError
  | ShaderCompilationError FilePath String
  | ProgramObjectError
  | ProgramLinkError String
  deriving (Eq, Show)

ppProgramError :: ProgramError -> String
ppProgramError (UniformNotFound name) = "Uniform variable \"" <> name <> "\" not found (possibly optimized away)"
ppProgramError ShaderObjectError = "Unable to create shader object"
ppProgramError ProgramObjectError = "Unable to create program object"
ppProgramError (ShaderCompilationError path log) = "Error while compiling " <> path <> ":\n" <> log
ppProgramError (ProgramLinkError log) = "Error while linking program:\n" <> log

data ProgramLog
  = LinkLog String
  | CompileLog FilePath String
  deriving (Eq, Show)

ppProgramLog :: ProgramLog -> String
ppProgramLog (LinkLog log) = "Warning while linking:\n" <> log
ppProgramLog (CompileLog path log) = "Warning while compiling " <> path <> ":\n" <> log

createProgram ::
  forall m mat vert.
  MonadIO m =>
  FilePath ->
  FilePath ->
  MaterialInitializer mat ->
  ExceptT ProgramError m (Program mat vert, [ProgramLog])
createProgram vert frag act = do
  (program, log) <- createShaderProgram vert frag
  glUseProgram program
  sets <- setters program
  pure (Program program sets (setter sets), log)
  where
    setters :: GLuint -> ExceptT ProgramError m (mat Setter)
    setters program = act $ \_ a lbl ->
      getUniformLocation lbl program >>= \case
        Nothing -> throwError $ UniformNotFound lbl
        Just loc -> do
          setUniformRaw loc a
          pure $ Setter $ setUniformRaw loc
    setter :: mat Setter -> Setter (mat Identity)
    setter matSet = Setter $ \mat ->
      void $ act $ \f _ _ -> runSetter (f matSet) (runIdentity $ f mat) $> Proxy

setUniform ::
  MonadIO m =>
  (forall f. mat f -> f a) ->
  a ->
  ProgramT mat v m ()
setUniform f a = ProgramT $ do
  (Program _ s _) <- ask
  liftIO $ runSetter (f s) a

setMaterial ::
  MonadIO m =>
  mat Identity ->
  ProgramT mat v m ()
setMaterial mat = ProgramT $ do
  Program _ _ (Setter s) <- ask
  liftIO $ s mat

createShaderProgram :: MonadIO m => FilePath -> FilePath -> ExceptT ProgramError m (GLuint, [ProgramLog])
createShaderProgram pathVert pathFrag = do
  (vert, vlog) <- createShader pathVert GL_VERTEX_SHADER
  (frag, flog) <- createShader pathFrag GL_FRAGMENT_SHADER
  let shaders = [vert, frag]
  programId <- glCreateProgram
  when (programId == 0) $ throwError ProgramObjectError
  linkSuccess <- liftIO $ do
    forM_ shaders $ glAttachShader programId
    glLinkProgram programId
    F.alloca $ \success -> do
      glGetProgramiv programId GL_LINK_STATUS success
      (/= 0) <$> F.peek success
  linkLog <- liftIO $ do
    bytes <- F.alloca $ \bytes ->
      glGetProgramiv programId GL_INFO_LOG_LENGTH bytes
        >> F.peek bytes
    F.allocaBytes (fromIntegral bytes) $ \res -> do
      glGetProgramInfoLog programId bytes F.nullPtr res
      F.peekCString res
  let log _ "" = []
      log f l = [f l]
      logs = log (CompileLog pathVert) vlog <> log (CompileLog pathFrag) flog <> log LinkLog linkLog
  if linkSuccess
    then do
      liftIO $ forM_ shaders glDeleteShader
      pure (programId, logs)
    else throwError (ProgramLinkError linkLog)

{-# ANN createShader "HLint: ignore Redundant do" #-}
createShader :: MonadIO m => FilePath -> GLenum -> ExceptT ProgramError m (GLenum, String)
createShader filename shaderType = do
  shaderId <- liftIO $ glCreateShader shaderType
  when (shaderId == 0) $ throwError ShaderObjectError
  shaderSrc <- liftIO $ readFile filename
  compileSuccess <- liftIO $ do
    F.withCString shaderSrc $ \src' ->
      F.with src' $ \src'' ->
        F.alloca $ \success -> do
          glShaderSource shaderId 1 src'' F.nullPtr
          glCompileShader shaderId
          glGetShaderiv shaderId GL_COMPILE_STATUS success
          (/= 0) <$> F.peek success
  compileLog <- liftIO $ do
    bytes <- F.alloca $ \bytes ->
      glGetShaderiv shaderId GL_INFO_LOG_LENGTH bytes
        >> F.peek bytes
    F.allocaBytes (fromIntegral bytes) $ \res ->
      glGetShaderInfoLog shaderId bytes F.nullPtr res
        >> F.peekCString res
  if compileSuccess
    then pure (shaderId, compileLog)
    else throwError (ShaderCompilationError filename compileLog)
