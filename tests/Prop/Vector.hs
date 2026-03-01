module Prop.Vector (tests) where

import Linear.V3 (V3)
import Linear.Vector (negated, zero, (*^), (^*), (^+^))
import Prop.V3 ()
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)

prop_lr_scalarproduct :: V3 Rational -> Rational -> Bool
prop_lr_scalarproduct v a = v ^* a == a *^ v

prop_negate_vector :: V3 Rational -> Bool
prop_negate_vector a = a ^+^ negated a == zero

tests :: [TestTree]
tests =
  [ testProperty "Left and right scalar product are equal" prop_lr_scalarproduct
  , testProperty "Negation is inverse under ^+^" prop_negate_vector
  ]
