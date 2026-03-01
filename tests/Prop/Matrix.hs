module Prop.Matrix (tests) where

import Linear.Matrix (M22, det22, inv22, M33, M44, transpose, (!*!), (!+!), identity, Trace(trace), (!!*), (*!!))
import Prop.V2 ()
import Prop.V3 ()
import Prop.V4 ()
import Test.QuickCheck (Property, (==>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

prop_m22inv :: M22 Rational -> Property
prop_m22inv a = (det22 a /= 0) ==> inv22 (inv22 a) == a

prop_m22transpose :: M22 Rational -> Bool
prop_m22transpose a = transpose (transpose a) == a

testsAddAssoc :: TestTree
testsAddAssoc = testGroup "associativity of !+!"
  [ testProperty "m22" (prop_addassoc :: M22 Rational -> M22 Rational -> M22 Rational -> Bool)
  , testProperty "m33" (prop_addassoc :: M33 Rational -> M33 Rational -> M33 Rational -> Bool)
  , testProperty "m44" (prop_addassoc :: M44 Rational -> M44 Rational -> M44 Rational -> Bool)
  ]
  where
      prop_addassoc a b c = ((a !+! b) !+! c) == (a !+! (b !+! c))

testsMulAssoc :: TestTree
testsMulAssoc = testGroup "associativity of !*!"
  [ testProperty "m22" (prop_mulassoc :: M22 Rational -> M22 Rational -> M22 Rational -> Bool)
  , testProperty "m33" (prop_mulassoc :: M33 Rational -> M33 Rational -> M33 Rational -> Bool)
  , testProperty "m44" (prop_mulassoc :: M44 Rational -> M44 Rational -> M44 Rational -> Bool)
  ]
  where
    prop_mulassoc a b c = ((a !*! b) !*! c) == (a !*! (b !*! c))

testsAddCommut :: TestTree
testsAddCommut = testGroup "commutativity of !+!"
  [ testProperty "m22" (prop_addcommut :: M22 Rational -> M22 Rational -> Bool)
  , testProperty "m33" (prop_addcommut :: M33 Rational -> M33 Rational -> Bool)
  , testProperty "m44" (prop_addcommut :: M44 Rational -> M44 Rational -> Bool)
  ]
  where
    prop_addcommut a b = (a !+! b) == (b !+! a)

prop_m22invmult :: M22 Rational -> M22 Rational -> Property
prop_m22invmult a b = det22 a /= 0 && det22 b /= 0 ==>
  (inv22 (a !*! b) == (inv22 b !*! inv22 a))

prop_m22invident :: M22 Rational -> Property
prop_m22invident a = det22 a /= 0 ==> a !*! inv22 a == identity

prop_m33multident :: M33 Rational -> Bool
prop_m33multident a = a !*! identity == a

prop_tracelinear :: M33 Rational -> M33 Rational -> Bool
prop_tracelinear a b = trace (a !+! b) == (trace a + trace b)

prop_tracetranspose :: M33 Rational -> Bool
prop_tracetranspose a = trace a == trace (transpose a)

prop_m33lrscalar :: M33 Rational -> Rational -> Bool
prop_m33lrscalar m a = m !!* a == a *!! m

prop_m33traceswap :: M33 Rational -> M33 Rational -> Bool
prop_m33traceswap a b = trace (a !*! b) == trace (b !*! a)



tests :: [TestTree]
tests =
  [ testGroup
      -- These tests don't rely on any specific size of matrix to function
      "general matrix operations"
      [ testProperty "transpose (transpose a) == a" prop_m22transpose
      , testsAddCommut
      , testsAddAssoc
      , testsMulAssoc
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
      , testProperty "(AB)^-1 == B^-1 * A^-1" prop_m22invmult
      ]
  ]
