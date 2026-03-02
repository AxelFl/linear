{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE CPP #-}
module Prop.Matrix (tests) where

import Linear.Matrix (M22, det22, inv22, transpose, (!*!), (!+!), identity, Trace(trace), (!!*), (*!!))
import Linear.Vector (Additive)
import Linear.V2 (V2)
import Linear.V3 (V3)
import Linear.V4 (V4)
import Prop.V2 ()
import Prop.V3 ()
import Prop.V4 ()
import Test.QuickCheck (Property, (==>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck ((.&&.))

import Data.Distributive (Distributive)

#define SQUAREMATRIX(testname) \
( testname @V2 @Rational .&&. \
  testname @V3 @Rational .&&. \
  testname @V4 @Rational \
)

#define ALLMATRIX(testname) \
( testname @V2 @V2 @Rational .&&. \
  testname @V2 @V3 @Rational .&&. \
  testname @V2 @V4 @Rational .&&. \
  testname @V3 @V2 @Rational .&&. \
  testname @V3 @V3 @Rational .&&. \
  testname @V3 @V3 @Rational .&&. \
  testname @V4 @V2 @Rational .&&. \
  testname @V4 @V3 @Rational .&&. \
  testname @V4 @V4 @Rational \
)

-- General block
prop_Transpose :: Property
prop_Transpose = ALLMATRIX(prop)
  where
    prop :: (Eq (m (n a)), Distributive m, Distributive n) => m (n a) -> Bool
    prop a = transpose (transpose a) == a

prop_AddAssoc :: Property
prop_AddAssoc = ALLMATRIX(prop_addassoc)
  where
    prop_addassoc :: (Eq (m (n a)), Additive m, Additive n, Num a) => m (n a) -> m (n a) -> m (n a) -> Bool
    prop_addassoc a b c = ((a !+! b) !+! c) == (a !+! (b !+! c))

prop_MulAssoc :: Property
prop_MulAssoc = SQUAREMATRIX(prop_mulassoc)
  where
    prop_mulassoc :: (Eq (m (m a)), Additive m, Foldable m, Num a) => m (m a) -> m (m a) -> m (m a) -> Bool
    prop_mulassoc a b c = ((a !*! b) !*! c) == (a !*! (b !*! c))

prop_AddCommut :: Property
prop_AddCommut = ALLMATRIX(prop_addcommut)
  where
    prop_addcommut :: (Eq (m (n a)), Additive m, Additive n, Num a) => m (n a) -> m (n a) -> Bool
    prop_addcommut a b = (a !+! b) == (b !+! a)

prop_IdentityNeutral :: Property
prop_IdentityNeutral = SQUAREMATRIX(prop)
  where
    prop :: (Eq (m (m a)), Additive m, Foldable m, Traversable m, Applicative m,  Num a) => m (m a) -> Bool
    prop a = a !*! identity == a

prop_TraceLinear :: Property
prop_TraceLinear = SQUAREMATRIX(prop)
  where
  prop :: (Trace m, Additive m, Num a, Eq a) => m (m a) -> m (m a) -> Bool
  prop a b = trace (a !+! b) == (trace a + trace b)

prop_TraceTranspose :: Property
prop_TraceTranspose = SQUAREMATRIX(prop)
  where
    prop :: (Trace m, Distributive m, Num a, Eq a) => m (m a) -> Bool
    prop a = trace a == trace (transpose a)

prop_LRScalar :: Property
prop_LRScalar = ALLMATRIX(prop)
  where
    prop :: (Functor m, Functor n, Num a, Eq (m (n a))) => m (n a) -> a -> Bool
    prop m a = m !!* a == a *!! m

prop_TraceSwap :: Property
prop_TraceSwap = SQUAREMATRIX(prop)
  where
    prop :: (Foldable m, Trace m, Additive m, Eq a, Num a) => m (m a) -> m (m a) -> Bool
    prop a b = trace (a !*! b) == trace (b !*! a)

-- 2x2 block
prop_m22inv :: M22 Rational -> Property
prop_m22inv a = (det22 a /= 0) ==> inv22 (inv22 a) == a

prop_m22invident :: M22 Rational -> Property
prop_m22invident a = det22 a /= 0 ==> a !*! inv22 a == identity

prop_m22invmult :: M22 Rational -> M22 Rational -> Property
prop_m22invmult a b = det22 a /= 0 && det22 b /= 0 ==>
  (inv22 (a !*! b) == (inv22 b !*! inv22 a))

tests :: [TestTree]
tests =
  [ testGroup
      -- These tests don't rely on any specific size of matrix to function
      "general matrix operations"
      [ testProperty "transpose (transpose a) == a" prop_Transpose
      , testProperty "commutativity of !+!" prop_AddCommut
      , testProperty "associativity of !+!" prop_AddAssoc
      , testProperty "associativity of !*!" prop_MulAssoc
      , testProperty "identity is neutral under !*!" prop_IdentityNeutral
      , testProperty "trace (a !+! b) == trace a + trace b" prop_TraceLinear
      , testProperty "trace a == trace (transpose a)" prop_TraceTranspose
      , testProperty "Left and right scalar product are equal" prop_LRScalar
      , testProperty "trace (a !*! b) == trace (b !*! a)" prop_TraceSwap
      ]
  , testGroup
      "2x2 matrix"
      [ testProperty "inv22 (inv22 a) == a" prop_m22inv
      , testProperty "a !*! inv a == I" prop_m22invident
      , testProperty "(AB)^-1 == B^-1 * A^-1" prop_m22invmult
      ]
  ]
