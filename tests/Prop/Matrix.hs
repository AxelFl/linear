module Prop.Matrix (tests) where

import Linear.Matrix (M22, det22, inv22, M33, transpose, (!*!), (!+!), identity, Trace(trace), (!!*), (*!!))
import Linear.Vector (Additive)
import Prop.V2 ()
import Prop.V3 ()
import Test.QuickCheck (Property, (==>), (.&&.))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

prop_m22inv :: M22 Rational -> Property
prop_m22inv a = (det22 a /= 0) ==> inv22 (inv22 a) == a

prop_m22transpose :: M22 Rational -> Bool
prop_m22transpose a = transpose (transpose a) == a

testsAddAssoc :: TestTree
testsAddAssoc = testGroup "associativity of !+!" [
     testProperty "m22" prop_m22addassoc
    ,testProperty "m33" prop_m33addassoc
  ]
  where
      prop_addassoc:: (Additive m, Additive n, Ord (m (n Rational))) => m (n Rational) -> m (n Rational) -> m (n Rational) -> Bool
      prop_addassoc a b c = ((a !+! b) !+! c) == (a !+! (b !+! c))

      prop_m22addassoc :: M22 Rational -> M22 Rational -> M22 Rational -> Bool
      prop_m22addassoc = prop_addassoc
      prop_m33addassoc :: M33 Rational -> M33 Rational -> M33 Rational -> Bool
      prop_m33addassoc = prop_addassoc

testsMulAssoc :: TestTree
testsMulAssoc = testGroup "associativity of !*!" [
     testProperty "m22" prop_m22mulassoc
    ,testProperty "m33" prop_m33mulassoc
  ]
  where
    prop_mulassoc:: (Additive m, Foldable m, Ord (m (m Rational))) => m (m Rational) -> m (m Rational) -> m (m Rational) -> Bool
    prop_mulassoc a b c = ((a !*! b) !*! c) == (a !*! (b !*! c))
    prop_m22mulassoc :: M22 Rational -> M22 Rational -> M22 Rational -> Bool
    prop_m22mulassoc = prop_mulassoc
    prop_m33mulassoc :: M33 Rational -> M33 Rational -> M33 Rational -> Bool
    prop_m33mulassoc = prop_mulassoc

testsAddCommut :: TestTree
testsAddCommut = testGroup "commutativity of !+!" [
     testProperty "m22" prop_m22addcommut
    ,testProperty "m33"prop_m33addcommut
  ]
  where
    prop_addcommut:: (Additive m, Additive n, Ord (m (n Rational))) => m (n Rational) -> m (n Rational) -> Bool
    prop_addcommut a b = (a !+! b) == (b !+! a)
    prop_m22addcommut :: M22 Rational -> M22 Rational -> Bool
    prop_m22addcommut = prop_addcommut
    prop_m33addcommut :: M33 Rational -> M33 Rational -> Bool
    prop_m33addcommut = prop_addcommut

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
