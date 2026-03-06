{-# OPTIONS_GHC -Wno-orphans #-}

module Prop.V2 (tests) where

import Linear.V2 (V2 (..), perp, angle, unangle)
import Linear.Epsilon (nearZero)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Arbitrary (..), forAll, choose)

instance (Arbitrary a) => Arbitrary (V2 a) where
  arbitrary = V2 <$> arbitrary <*> arbitrary

prop_quadPerp :: V2 Rational -> Bool
prop_quadPerp a = (perp . perp . perp . perp) a == a

prop_unangleInverse :: Double -> Bool
prop_unangleInverse a = nearZero ((unangle . angle) a - a)

tests :: [TestTree]
tests =
  [ testProperty "4 perpendicular is unchanged" prop_quadPerp
  , testProperty "unangle is inverse of angle" $ forAll (choose (-pi / 2 , 3/2 * pi - 1e-10 )) prop_unangleInverse
  ]
