{-# LANGUAGE OverloadedStrings #-}

module Obj where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text as P
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Word (Word32)
import Linear

loadObj :: FilePath -> IO (Either String ObjFile)
loadObj path = parseOnly parseObj <$> T.readFile path

loadMat :: FilePath -> IO (Either String [Material])
loadMat path = parseOnly parseMtl <$> T.readFile path

data ObjFile
  = ObjFile
      { fileMtl :: String,
        fileObjs :: [Object]
      }
  deriving (Eq, Show)

data Object
  = Object
      { objName :: String,
        objMaterial :: String,
        objVertices :: [V3 Float],
        objNormals :: [V3 Float],
        objTexCoords :: [V2 Float],
        objFaces :: [M33 Word32]
      }
  deriving (Eq, Show)

data Material
  = Material
      { mtlName :: String,
        mtlExponent :: Float,
        mtlAmbientRGB :: V3 Float,
        mtlDiffuseRGB :: V3 Float,
        mtlSpecularRGB :: V3 Float,
        mtlOpticalDensity :: Float,
        mtlDissolve :: Float,
        mtlIllum :: Word32,
        mtlMapDiffuse :: Maybe FilePath,
        mtlMapBump :: Maybe FilePath,
        mtlMapSpecular :: Maybe FilePath
      }
  deriving (Eq, Show)

parseMtl :: Parser [Material]
parseMtl = do
  skipMany comment
  many $ do
    skipMany endOfLine
    Material
      <$> newmtl
      <*> exponent
      <*> ambientRGB
      <*> diffuseRGB
      <*> specularRGB
      <*> density
      <*> dissolve
      <*> illum
      <*> optional mapDiff
      <*> optional mapBump
      <*> optional mapSpec
  where
    newmtl :: Parser String
    newmtl = string "newmtl" *> name <* endOfLine
    ambientRGB = string "Ka" *> v3 <* endOfLine
    diffuseRGB = string "Kd" *> v3 <* endOfLine
    specularRGB = string "Ks" *> v3 <* endOfLine
    density = string "Ni" *> float <* endOfLine
    exponent = string "Ns" *> float <* endOfLine
    dissolve = char 'd' *> float <* endOfLine
    mapDiff = string "map_Kd" *> name <* endOfLine
    mapBump = string "map_Bump" *> name <* endOfLine
    mapSpec = string "map_Ks" *> name <* endOfLine
    illum = string "illum" *> word <* endOfLine

-- A space followed by 3 floating point values
v3 :: Parser (V3 Float)
v3 = liftA3 V3 float float float

-- A space followed by 2 floating point values
v2 :: Parser (V2 Float)
v2 = liftA2 V2 float float

hSpace :: Parser ()
hSpace =
  skipMany1 $
    void (satisfy isHorizontalSpace) <|> (char '\\' >> endOfLine)

name :: Parser String
name = hSpace *> (T.unpack <$> P.takeWhile (not . isSpace))

float :: Parser Float
float = hSpace *> (realToFrac <$> double)

word :: Parser Word32
word = hSpace *> decimal

comment :: Parser ()
comment = char '#' *> skipWhile (not . isEndOfLine) *> endOfLine

parseObj :: Parser ObjFile
parseObj = do
  skipMany comment <?> "Comment"
  file <- mtlLib <?> "MtlLib"
  objs <- many $ do
    obj <- object
    vs <- many vertex <?> (obj <> " vertices")
    vts <- many textureCoords <?> (obj <> " textures coords")
    vns <- many normal <?> (obj <> " vertex normals")
    mtl <- useMtl <?> (obj <> " material")
    _ <- group <?> (obj <> " group")
    faces <- many face <?> (obj <> " material")
    pure $ Object obj mtl vs vns vts faces
  pure $ ObjFile file objs
  where
    vertex = char 'v' *> v3 <* endOfLine
    textureCoords = string "vt" *> v2 <* endOfLine
    normal = string "vn" *> v3 <* endOfLine
    group = char 's' *> hSpace *> decimal <* endOfLine
    object = char 'o' *> hSpace *> name <* endOfLine
    useMtl = string "usemtl" *> hSpace *> name <* endOfLine
    mtlLib = string "mtllib" *> hSpace *> name <* endOfLine
    face1 :: Parser (V3 Word32)
    face1 = do
      fv <- decimal <* char '/'
      ft <- decimal <* char '/'
      fn <- decimal
      pure (V3 fv ft fn)
    face :: Parser (M33 Word32)
    face = do
      _ <- char 'f'
      let p = hSpace >> face1
      V3 <$> p <*> p <*> p <* endOfLine
