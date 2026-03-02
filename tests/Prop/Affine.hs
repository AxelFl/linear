module Prop.Affine (tests) where

import Linear.Affine (Affine(..))
import Linear.V3 (V3)
import Linear.Vector ((^+^))
import Prop.V3 ()
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)

prop_1 :: V3 Rational -> V3 Rational -> Bool
prop_1 a b = a .+^ (b .-. a) == b

prop_2 :: V3 Rational -> V3 Rational -> V3 Rational -> Bool
prop_2 a u v = (a .+^ u) .+^ v == a .+^ (u ^+^ v)

prop_3 :: V3 Rational -> V3 Rational -> V3 Rational -> Bool
prop_3 a b v = (a .-. b) .+^ v == (a .+^ v) .-. b

tests :: [TestTree]
tests =
  [ testProperty "prop 1" prop_1
  , testProperty "prop 2" prop_2
  , testProperty "prop 3" prop_3
  ]
