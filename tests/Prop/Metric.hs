{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE CPP #-}

module Prop.Metric (tests) where

import Linear.Metric (Metric, dot, quadrance, norm)
import Linear.V1 (V1(..))
import Linear.V2 (V2(..))
import Linear.V3 (V3(..))
import Linear.V4 (V4(..))
import Linear.Vector ((^+^), (^-^))
import Prop.V1 ()
import Prop.V2 ()
import Prop.V3 ()
import Prop.V4 ()
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck ((.&&.))

#define ALLVECTORS(testname) \
( prop @V1 @Rational .&&. \
  prop @V2 @Rational .&&. \
  prop @V3 @Rational .&&. \
  prop @V4 @Rational \
)

testsDotSelf :: TestTree
testsDotSelf = testProperty "dot product is norm squared"
  ALLVECTORS(prop)
  where
    prop :: (Metric v, Num a, Eq a) => v a -> Bool
    prop a = a `dot` a == quadrance a

testsDotCommut :: TestTree
testsDotCommut = testProperty "Commutativity of scalar product"
  ALLVECTORS(prop)
  where
    prop :: (Metric v, Num a, Eq a) => v a -> v a -> Bool
    prop a b = a `dot` b == b `dot` a

testsDotDist :: TestTree
testsDotDist = testProperty "Distributivity of scalar product over addition"
  ALLVECTORS(prop)
  where
    prop :: (Metric v, Num a, Eq a) => v a -> v a -> v a -> Bool
    prop a b c = (a ^+^ b) `dot` c == (a `dot` c) + (b `dot` c) 

testsCSIeq :: TestTree
testsCSIeq = testProperty "Cauchy–Schwarz inequality"
    ( prop @V1 @Double .&&. 
    prop @V2 @Double .&&. 
    prop @V3 @Double .&&. 
    prop @V4 @Double 
    )
  where
    prop :: (Metric v, Floating a, Ord a) => v a -> v a -> Bool
    prop a b = a `dot` b <= norm a * norm b

testsTriIeq :: TestTree
testsTriIeq = testProperty "Triangle Inequality"
    ( prop @V1 @Double .&&. 
    prop @V2 @Double .&&. 
    prop @V3 @Double .&&. 
    prop @V4 @Double 
    )
  where
    prop :: (Metric v, Floating a, Ord a) => v a -> v a -> Bool
    prop a b =  norm (a ^+^ b) <= norm a + norm b
testsInvTriIeq :: TestTree
testsInvTriIeq = testProperty "Inverse Triangle Inequality"
    ( prop @V1 @Double .&&. 
    prop @V2 @Double .&&. 
    prop @V3 @Double .&&. 
    prop @V4 @Double 
    )
  where
    prop :: (Metric v, Floating a, Ord a) => v a -> v a -> Bool
    prop a b =  norm (a ^-^ b) >= norm a - norm b
tests :: [TestTree]
tests =
  [ testsDotSelf
  , testsDotCommut
  , testsDotDist
  , testsCSIeq
  , testsTriIeq
  , testsInvTriIeq
  ]
