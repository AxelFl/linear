module Prop.Vector (tests, additiveAssoc) where

import Linear.V3 (V3)
import Linear.Vector (Additive, negated, zero, (*^), (^*), (^+^))
import Prop.V3 ()
import Test.QuickCheck (Arbitrary)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)

additiveAssoc :: (Additive f, Num a, Arbitrary a, Eq (f a)) => (f a -> f a -> f a) -> f a -> f a -> f a -> Bool
additiveAssoc op a b c = ((a `op` b) `op` c == a `op` (b `op` c))

prop_lr_scalarproduct :: V3 Rational -> Rational -> Bool
prop_lr_scalarproduct v a = v ^* a == a *^ v

prop_negate_vector :: V3 Rational -> Bool
prop_negate_vector a = a ^+^ negated a == zero

tests :: [TestTree]
tests =
  [ testProperty "Left and right scalar product are equal" prop_lr_scalarproduct
  , testProperty "Negation is inverse under ^+^" prop_negate_vector
  ]
