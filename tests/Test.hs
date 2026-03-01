{-# LANGUAGE CPP #-}
module Main (main) where

import Test.Tasty (defaultMain, testGroup, TestTree)

import qualified Prop.Quaternion
import qualified Prop.Matrix
import qualified Prop.Vector
import qualified Prop.V3
import qualified Unit.Binary
import qualified Unit.Plucker
import qualified Unit.V

tests :: [TestTree]
tests =
  [ testGroup "Property tests"
    [ testGroup "Quaternion" Prop.Quaternion.tests
    , testGroup "Matrix" Prop.Matrix.tests
    , testGroup "Vector" Prop.Vector.tests
    , testGroup "V3" Prop.V3.tests
    ]
  , testGroup "Unit tests"
    [ testGroup "Binary" Unit.Binary.tests
    , testGroup "Plucker" Unit.Plucker.tests
    , testGroup "V" Unit.V.tests
    ]
  ]

main :: IO ()
main = defaultMain $ testGroup "linear" tests
