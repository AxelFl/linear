{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}

module Prop.Metric (tests) where

import Linear.Metric (Metric, dot, norm)
import Linear.V1 (V1 (..))
import Linear.V2 (V2 (..))
import Linear.V3 (V3 (..))
import Linear.V4 (V4 (..))
import Linear.Vector ((^+^), (^-^))
import Prop.V1 ()
import Prop.V2 ()
import Prop.V3 ()
import Prop.V4 ()
import Test.QuickCheck (Property, (.&&.))
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)

#define ALLVECTORS(testname) \
  prop @V1 @Rational .&&. \
  prop @V2 @Rational .&&. \
  prop @V3 @Rational .&&. \
  prop @V4 @Rational

prop_DotCommut :: Property
prop_DotCommut = ALLVECTORS (prop)
 where
  prop :: (Metric v, Num a, Eq a) => v a -> v a -> Bool
  prop a b = a `dot` b == b `dot` a

prop_DotDist :: Property
prop_DotDist = ALLVECTORS (prop)
 where
  prop :: (Metric v, Num a, Eq a) => v a -> v a -> v a -> Bool
  prop a b c = (a ^+^ b) `dot` c == (a `dot` c) + (b `dot` c)

prop_CSIneq :: Property
prop_CSIneq =
  prop @V1 @Double
    .&&. prop @V2 @Double
    .&&. prop @V3 @Double
    .&&. prop @V4 @Double
 where
  prop :: (Metric v, Floating a, Ord a) => v a -> v a -> Bool
  prop a b = a `dot` b <= norm a * norm b + 1e-12

prop_TriangleIneq :: Property
prop_TriangleIneq =
  prop @V1 @Double
    .&&. prop @V2 @Double
    .&&. prop @V3 @Double
    .&&. prop @V4 @Double
 where
  prop :: (Metric v, Floating a, Ord a) => v a -> v a -> Bool
  prop a b = norm (a ^+^ b) <= norm a + norm b + 1e-12

prop_InvTriangleIneq :: Property
prop_InvTriangleIneq =
  prop @V1 @Double
    .&&. prop @V2 @Double
    .&&. prop @V3 @Double
    .&&. prop @V4 @Double
 where
  prop :: (Metric v, Floating a, Ord a) => v a -> v a -> Bool
  prop a b = norm (a ^-^ b) + 1e-12 >= norm a - norm b

tests :: [TestTree]
tests =
  [ testProperty "Commutativity of scalar product" prop_DotCommut
  , testProperty "Distributivity of scalar product over addition" prop_DotDist
  , testProperty "Cauchy–Schwarz inequality" prop_CSIneq
  , testProperty "Triangle Inequality" prop_TriangleIneq
  , testProperty "Inverse Triangle Inequality" prop_InvTriangleIneq
  ]
