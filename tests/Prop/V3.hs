{-# OPTIONS_GHC -Wno-orphans #-}

module Prop.V3 (tests) where

import Linear.Metric (dot)
import Linear.V3 (V3 (..), cross)
import Linear.Vector (zero, (*^), (^-^))
import Test.QuickCheck (Arbitrary (..))
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)

instance (Arbitrary a) => Arbitrary (V3 a) where
  arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary

prop_crossinv :: V3 Rational -> V3 Rational -> Bool
prop_crossinv a b = a `cross` b == -(b `cross` a)

prop_selfcross :: V3 Rational -> Bool
prop_selfcross a = a `cross` a == zero

prop_distcross :: V3 Rational -> V3 Rational -> V3 Rational -> Bool
prop_distcross a b c = (a + b) `cross` c == (a `cross` c) + (b `cross` c)

prop_scalartripleprod :: V3 Rational -> V3 Rational -> V3 Rational -> Bool
prop_scalartripleprod a b c = a `dot` (b `cross` c) == b `dot` (c `cross` a)

prop_vectortripleprod :: V3 Rational -> V3 Rational -> V3 Rational -> Bool
prop_vectortripleprod a b c =
  (a `cross` (b `cross` c)) == ((a `dot` c) *^ b) ^-^ ((a `dot` b) *^ c)

tests :: [TestTree]
tests =
  [ testProperty "a x b == - (b x a)" prop_crossinv
  , testProperty "a x a == 0" prop_selfcross
  , testProperty "(a + b) x c == a x c + b x c" prop_distcross
  , testProperty "Scalar triple product" prop_scalartripleprod
  , testProperty "Vector triple product" prop_vectortripleprod
  ]
