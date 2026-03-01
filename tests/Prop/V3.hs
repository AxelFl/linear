{-# OPTIONS_GHC -Wno-orphans #-}
module Prop.V3 (tests) where

import Linear.V3 (V3(..), cross)
import Linear.Vector (zero)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Arbitrary(..))

instance Arbitrary a => Arbitrary (V3 a) where
  arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary

prop_crossinv :: V3 Double -> V3 Double -> Bool
prop_crossinv a b = a `cross` b == - (b `cross` a)

prop_selfcross :: V3 Double -> Bool
prop_selfcross a = a `cross` a == zero

tests :: [TestTree]
tests = 
  [ testProperty "a x b == - (b x a)" prop_crossinv
  , testProperty "a x a == 0" prop_selfcross
  ]
