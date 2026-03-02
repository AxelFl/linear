{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE CPP #-}

module Prop.Vector (tests) where

import Linear.V1 (V1)
import Linear.V2 (V2)
import Linear.V3 (V3)
import Linear.V4 (V4)
import Linear.Vector (Additive(..), negated, zero, (*^), (^*))
import Prop.V1 ()
import Prop.V2 ()
import Prop.V3 ()
import Prop.V4 ()
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck ((.&&.))

-- This macro avoids having these lines in all tests that
-- want to run on all vectors. It is a bit evil but we think
-- this is the simplest solution.
#define TESTALLVECTOR(testname) \
( testname @V1 @Rational .&&. \
  testname @V2 @Rational .&&. \
  testname @V3 @Rational .&&. \
  testname @V4 @Rational \
)

testsAddAssoc :: TestTree
testsAddAssoc = testProperty "associativity of ^+^"
  TESTALLVECTOR(prop)
  where
    prop :: (Eq (v a), Additive v, Num a) => v a -> v a -> v a -> Bool
    prop a b c = ((a ^+^ b) ^+^ c) == (a ^+^ (b ^+^ c))

testsAddCommut :: TestTree
testsAddCommut = testProperty "commutativity of ^+^"
  TESTALLVECTOR(prop)
  where
    prop :: (Eq (v a), Additive v, Num a) => v a -> v a -> Bool
    prop a b = (a ^+^ b) == (b ^+^ a)

testsLRScalarProduct :: TestTree
testsLRScalarProduct = testProperty "Left and right scalar product are equal"
  TESTALLVECTOR(prop)
  where
    prop :: (Eq (v a), Functor v, Num a) => v a -> a -> Bool
    prop v a = v ^* a == a *^ v

testsDistScalarR :: TestTree
testsDistScalarR = testProperty "Right scalar are distributive over vector addition"
  TESTALLVECTOR(prop)
  where
    prop :: (Eq (v a), Additive v, Num a) => v a -> v a -> a -> Bool
    prop a b c = (a ^+^ b) ^* c == (a^*c) ^+^ (b^*c)

testsDistScalarL :: TestTree
testsDistScalarL = testProperty "Left scalar are distributive over vector addition"
  TESTALLVECTOR(prop)
  where
    prop :: (Eq (v a), Additive v, Num a) => v a -> v a -> a -> Bool
    prop a b c = c *^ (a ^+^ b) == (c*^a) ^+^ (c*^b)

testsNegateVector :: TestTree
testsNegateVector = testProperty "Negation is inverse under ^+^"
  TESTALLVECTOR(prop)
  where
    prop :: (Eq (v a), Additive v, Num a) => v a -> Bool
    prop a = (a ^+^ negated a) == zero

tests :: [TestTree]
tests =
  [ testsLRScalarProduct
  , testsDistScalarR
  , testsDistScalarL
  , testsNegateVector
  , testsAddAssoc
  , testsAddCommut
  ]
