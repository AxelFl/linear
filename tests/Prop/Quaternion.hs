{-# OPTIONS_GHC -Wno-orphans #-}

module Prop.Quaternion (tests) where

import Linear.Conjugate
import Linear.Epsilon (nearZero)
import Linear.Metric (signorm)
import Linear.Quaternion (Quaternion (..), rotate)
import Linear.V3 (V3 (..))
import Linear.Vector (lerp)
import Test.QuickCheck (Arbitrary (..), Property, (==>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Prop.V3 ()

instance (Arbitrary a) => Arbitrary (Quaternion a) where
  arbitrary = Quaternion <$> arbitrary <*> arbitrary

prop_lerp0 :: Quaternion Double -> Quaternion Double -> Bool
prop_lerp0 a b = nearZero (lerp 0 a b - a)

prop_lerp1 :: Quaternion Double -> Quaternion Double -> Bool
prop_lerp1 a b = nearZero (lerp 1 a b - b)

prop_rotateInverse :: Quaternion Double -> V3 Double -> Property
prop_rotateInverse q v = q /= 0 ==> nearZero (rotate (conjugate qn) (rotate qn v) - v)
 where
  qn = signorm q

tests :: [TestTree]
tests =
  [ testGroup
      "lerp"
      [ testProperty "lerp 0 a b == a" prop_lerp0
      , testProperty "lerp 1 a b == b" prop_lerp1
      ]
  , testProperty "conjugate quaternion creates inverse rotation" prop_rotateInverse
  ]
