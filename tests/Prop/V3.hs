{-# OPTIONS_GHC -Wno-orphans #-}

module Prop.V3 (tests) where

import Linear.Metric (dot)
import Linear.V3 (V3 (..), cross, triple)
import Linear.Vector (zero, (*^), (^-^), (^+^))
import Test.QuickCheck (Arbitrary (..))
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)

instance (Arbitrary a) => Arbitrary (V3 a) where
  arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary

prop_crossInv :: V3 Rational -> V3 Rational -> Bool
prop_crossInv a b = a `cross` b == -(b `cross` a)

prop_selfCross :: V3 Rational -> Bool
prop_selfCross a = a `cross` a == zero

prop_distCross :: V3 Rational -> V3 Rational -> V3 Rational -> Bool
prop_distCross a b c = (a ^+^ b) `cross` c == (a `cross` c) ^+^ (b `cross` c)

prop_vectorTripleprod :: V3 Rational -> V3 Rational -> V3 Rational -> Bool
prop_vectorTripleprod a b c =
  (a `cross` (b `cross` c)) == ((a `dot` c) *^ b) ^-^ ((a `dot` b) *^ c)

prop_tripleCircular :: V3 Rational -> V3 Rational -> V3 Rational -> Bool
prop_tripleCircular a b c = triple a b c == triple b c a

prop_tripleSwap :: V3 Rational -> V3 Rational -> V3 Rational -> Bool
prop_tripleSwap a b c = triple a b c == - triple a c b

tests :: [TestTree]
tests =
  [ testProperty "a x b == - (b x a)" prop_crossInv
  , testProperty "a x a == 0" prop_selfCross
  , testProperty "(a + b) x c == a x c + b x c" prop_distCross
  , testProperty "vector triple product" prop_vectorTripleprod
  , testProperty "triple a b c = triple b c a" prop_tripleCircular
  , testProperty "triple a b c = -triple a c b" prop_tripleSwap
  ]
