{-# OPTIONS_GHC -Wno-orphans #-}
module Prop.Matrix (tests) where

import Linear.Matrix (M22, inv22, det22, transpose)
import Linear.Epsilon (nearZero)
import Test.QuickCheck (Property, (==>))
import Prop.V2 ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

prop_m22inv :: M22 Double -> Property
prop_m22inv a = (det22 a /= 0) ==> nearZero (inv22 (inv22 a) - a)

prop_m22transpose :: M22 Double -> Bool
prop_m22transpose a = transpose (transpose a) == a

prop_m22addcommut :: M22 Double -> M22 Double -> Bool
prop_m22addcommut a b = nearZero (( a + b ) - (b + a))

prop_m22addassoc :: M22 Double -> M22 Double -> M22 Double -> Bool
prop_m22addassoc a b c = nearZero ((a + ( b + c )) - (( a + b ) + c))

prop_m22multassoc :: M22 Double -> M22 Double -> M22 Double -> Bool
prop_m22multassoc a b c = nearZero ((a * ( b * c )) - (( a * b ) * c))

tests :: [TestTree]
tests =
  [ testGroup "matrix" 
    [testGroup "2x2 matrix"
      [ testProperty "inv22 (inv22 a) == a" prop_m22inv
      , testProperty "transpose (transpose a) == a" prop_m22transpose
      , testProperty "commutativity of +" prop_m22addcommut
      , testProperty "associativity of +" prop_m22addassoc
      , testProperty "associativity of *" prop_m22multassoc
      ]
    ]
  ]
