{-# OPTIONS_GHC -Wno-orphans #-}

module Prop.V2 (tests) where

import Linear.V2 (V2 (..), perp, angle, unangle)
import Linear.Epsilon (nearZero)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Arbitrary (..), forAll, choose)

instance (Arbitrary a) => Arbitrary (V2 a) where
  arbitrary = V2 <$> arbitrary <*> arbitrary

prop_QuadPerp :: V2 Rational -> Bool
prop_QuadPerp a = (perp . perp . perp . perp) a == a

prop_UnangleInverse :: Double -> Bool
prop_UnangleInverse a = nearZero ((unangle . angle) a - a)

tests :: [TestTree]
tests =
  [ testProperty "4 perpendicular is unchanged" prop_QuadPerp
  , testProperty "Unangle is inverse of angle" $ forAll (choose (-pi / 2 , 5/4 * pi - 1e-12 )) prop_UnangleInverse
  ]
