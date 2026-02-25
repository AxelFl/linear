{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Prop.Matrix (tests) where

import Linear.Matrix (M22(..), inv22, det22)
import Linear.V2 (V2(..))
import Linear.Epsilon (nearZero)
import Test.QuickCheck (Arbitrary(..), Property, (==>))
import Prop.V2
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

prop_m22inv :: M22 Double -> Property
prop_m22inv a = (det22 a /= 0) ==> nearZero (inv22 (inv22 a) - a)

tests :: [TestTree]
tests =
  [ testGroup "matrix" 
    [ testProperty "m22inv (m22inv a) == a" prop_m22inv
    ]
  ]
