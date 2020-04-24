{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.Map as M
import qualified Data.Vector.Storable as VS
import Mesh
import Test.Tasty
import Test.Tasty.QuickCheck as QC

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties =
  testGroup
    "QuickCheck Properties"
    [ QC.testProperty "toVertexList . mkMesh" $
        \(is :: [Float]) -> is == toVertexList (mkMesh is)
    ]
