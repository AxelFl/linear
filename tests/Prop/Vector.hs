{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}

module Prop.Vector (tests) where

import Linear.V1 (V1)
import Linear.V2 (V2)
import Linear.V3 (V3)
import Linear.V4 (V4)
import Linear.Vector (Additive (..), negated, zero, (*^), (^*))
import Prop.V1 ()
import Prop.V2 ()
import Prop.V3 ()
import Prop.V4 ()
import Test.QuickCheck (Property, (.&&.))
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)

-- This macro avoids having these lines in all tests that
-- want to run on all vectors. It is a bit evil but we think
-- this is the simplest solution.
#define ALLVECTORS(testname) \
  testname @V1 @Rational .&&. \
  testname @V2 @Rational .&&. \
  testname @V3 @Rational .&&. \
  testname @V4 @Rational

prop_AddAssoc :: Property
prop_AddAssoc = ALLVECTORS (prop)
 where
  prop :: (Eq (v a), Additive v, Num a) => v a -> v a -> v a -> Bool
  prop a b c = ((a ^+^ b) ^+^ c) == (a ^+^ (b ^+^ c))

prop_AddCommut :: Property
prop_AddCommut = ALLVECTORS (prop)
 where
  prop :: (Eq (v a), Additive v, Num a) => v a -> v a -> Bool
  prop a b = (a ^+^ b) == (b ^+^ a)

prop_LRScalarProduct :: Property
prop_LRScalarProduct = ALLVECTORS (prop)
 where
  prop :: (Eq (v a), Functor v, Num a) => v a -> a -> Bool
  prop v a = v ^* a == a *^ v

prop_DistScalarR :: Property
prop_DistScalarR = ALLVECTORS (prop)
 where
  prop :: (Eq (v a), Additive v, Num a) => v a -> v a -> a -> Bool
  prop a b c = (a ^+^ b) ^* c == (a ^* c) ^+^ (b ^* c)

prop_DistScalarL :: Property
prop_DistScalarL = ALLVECTORS (prop)
 where
  prop :: (Eq (v a), Additive v, Num a) => v a -> v a -> a -> Bool
  prop a b c = c *^ (a ^+^ b) == (c *^ a) ^+^ (c *^ b)

prop_NegateVector :: Property
prop_NegateVector = ALLVECTORS (prop)
 where
  prop :: (Eq (v a), Additive v, Num a) => v a -> Bool
  prop a = (a ^+^ negated a) == zero

tests :: [TestTree]
tests =
  [ testProperty "Left and right scalar product are equal" prop_LRScalarProduct
  , testProperty "Right scalar are distributive over vector addition" prop_DistScalarR
  , testProperty "Left scalar are distributive over vector addition" prop_DistScalarL
  , testProperty "Negation is inverse under ^+^" prop_NegateVector
  , testProperty "associativity of ^+^" prop_AddAssoc
  , testProperty "commutativity of ^+^" prop_AddCommut
  ]
