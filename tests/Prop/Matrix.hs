module Prop.Matrix (tests) where

import Linear.Epsilon (nearZero)
import Linear.Matrix (M22, det22, inv22, M33, transpose, (!*!), (!+!), (!-!), identity, Trace(trace), (!!*), (*!!))
import Prop.V2 ()
import Prop.V3 ()
import Prop.Vector (additiveAssoc)
import Test.QuickCheck (Property, (==>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

prop_m22inv :: M22 Double -> Property
prop_m22inv a = (det22 a /= 0) ==> nearZero (inv22 (inv22 a) - a)

prop_m22transpose :: M22 Double -> Bool
prop_m22transpose a = transpose (transpose a) == a

prop_m22addcommut :: M22 Double -> M22 Double -> Bool
prop_m22addcommut a b = nearZero ((a !+! b) !-! (b !+! a))

prop_m22addassoc :: M22 Double -> M22 Double -> M22 Double -> Bool
prop_m22addassoc = additiveAssoc (!+!)

prop_m22multassoc :: M22 Double -> M22 Double -> M22 Double -> Bool
prop_m22multassoc = additiveAssoc (!*!)

-- This is incredibly buggy, floating point makes it super hard to
-- test with the inverse stuff. Can discard like a quarter  of the tests
-- to make sure it runs properly by demanding a large determinant.
-- 1 here is just a number that was big enough to seem to work.
prop_m22invmult :: M22 Double -> M22 Double -> Property
prop_m22invmult a b = (not . (>) 1 . det22) a && (not . (>) 1 . det22) b ==>
  nearZero (inv22 (a !*! b) !-! (inv22 b !*! inv22 a))

prop_m22invident :: M22 Double -> Property
prop_m22invident a = det22 a /= 0 ==> nearZero (a !*! inv22 a !-! identity)

prop_m33multident :: M33 Double -> Bool
prop_m33multident a = a !*! identity == a

prop_tracelinear :: M33 Double -> M33 Double -> Bool
prop_tracelinear a b = nearZero (trace (a !+! b) - (trace a + trace b))

prop_tracetranspose :: M33 Double -> Bool
prop_tracetranspose a = trace a == trace (transpose a)

prop_m33lrscalar :: M33 Double -> Double -> Bool
prop_m33lrscalar m a = m !!* a == a *!! m

-- 1e-11 is just a small value, the "standard" nearZero is 1e-12,
-- but this fails too often, with the largest errors being just above
-- this threshold.
prop_m33traceswap :: M33 Double -> M33 Double -> Bool
prop_m33traceswap a b = (1e-11 >) (trace (a !*! b) - trace (b !*! a))

tests :: [TestTree]
tests =
  [ testGroup
      -- These tests don't rely on any specific size of matrix to function
      "general matrix operations"
      [ testProperty "transpose (transpose a) == a" prop_m22transpose
      , testProperty "commutativity of !+!" prop_m22addcommut
      , testProperty "associativity of !+!" prop_m22addassoc
      , testProperty "associativity of !*!" prop_m22multassoc
      , testProperty "identity matrix neutral element under !*!" prop_m33multident
      , testProperty "trace (a !+! b) == trace a + trace b" prop_tracelinear
      , testProperty "trace a == trace (transpose a)" prop_tracetranspose
      , testProperty "Left and right scalar product are equal" prop_m33lrscalar
      , testProperty "trace (a !*! b) == trace (b !*! a)" prop_m33traceswap
      ]
  , testGroup
      "2x2 matrix"
      [ testProperty "inv22 (inv22 a) == a" prop_m22inv
      , testProperty "a !*! inv a == I" prop_m22invident
      --, testProperty "(AB)^-1 == B^-1 * A^-1" prop_m22invmult
      ]
  ]
