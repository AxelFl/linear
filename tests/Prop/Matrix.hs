{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}

module Prop.Matrix (tests) where

import Linear.Matrix (
  InvertibleMatrix (..),
  Trace (trace),
  identity,
  transpose,
  (!!*),
  (!*!),
  (!+!),
  (*!!),
 )
import Linear.V2 (V2)
import Linear.V3 (V3)
import Linear.V4 (V4)
import Linear.Vector (Additive)
import Prop.V2 ()
import Prop.V3 ()
import Prop.V4 ()
import Test.QuickCheck (Property, (.&&.), (==>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Data.Distributive (Distributive)

#define SQUAREMATRIX(testname) \
  testname @V2 @Rational .&&. \
  testname @V3 @Rational .&&. \
  testname @V4 @Rational

#define ALLMATRIX(testname) \
  testname @V2 @V2 @Rational .&&. \
  testname @V2 @V3 @Rational .&&. \
  testname @V2 @V4 @Rational .&&. \
  testname @V3 @V2 @Rational .&&. \
  testname @V3 @V3 @Rational .&&. \
  testname @V3 @V3 @Rational .&&. \
  testname @V4 @V2 @Rational .&&. \
  testname @V4 @V3 @Rational .&&. \
  testname @V4 @V4 @Rational

-- Properties of general matrices
prop_Transpose :: Property
prop_Transpose = ALLMATRIX (prop)
 where
  prop :: (Eq (m (n a)), Distributive m, Distributive n) => m (n a) -> Bool
  prop a = transpose (transpose a) == a

prop_TransposeDistAdd :: Property
prop_TransposeDistAdd = ALLMATRIX (prop)
 where
  prop ::
    ( Eq (n (m a))
    , Additive m
    , Distributive m
    , Distributive n
    , Additive n
    , Num a
    ) =>
    m (n a) ->
    m (n a) ->
    Bool
  prop a b = transpose (a !+! b) == (transpose a !+! transpose b)

prop_TransposeDistMul :: Property
prop_TransposeDistMul = prop @V2 @V2 @Rational .&&. prop @V2 @V2 @Rational
 where
  prop ::
    ( Additive m
    , Foldable m
    , Distributive m
    , Distributive n
    , Foldable n
    , Additive n
    , Num a
    , Eq (m (m a))
    ) =>
    m (n a) ->
    n (m a) ->
    Bool
  prop a b = transpose (a !*! b) == (transpose b !*! transpose a)

prop_AddAssoc :: Property
prop_AddAssoc = ALLMATRIX (prop)
 where
  prop :: (Eq (m (n a)), Additive m, Additive n, Num a) => m (n a) -> m (n a) -> m (n a) -> Bool
  prop a b c = ((a !+! b) !+! c) == (a !+! (b !+! c))

prop_AddCommut :: Property
prop_AddCommut = ALLMATRIX (prop)
 where
  prop :: (Eq (m (n a)), Additive m, Additive n, Num a) => m (n a) -> m (n a) -> Bool
  prop a b = (a !+! b) == (b !+! a)

prop_LRScalar :: Property
prop_LRScalar = ALLMATRIX (prop)
 where
  prop :: (Functor m, Functor n, Num a, Eq (m (n a))) => m (n a) -> a -> Bool
  prop m a = m !!* a == a *!! m

-- Properties of square matrices
prop_MulAssoc :: Property
prop_MulAssoc = SQUAREMATRIX (prop)
 where
  prop :: (Eq (m (m a)), Additive m, Foldable m, Num a)
    => m (m a) -> m (m a) -> m (m a) -> Bool
  prop a b c = ((a !*! b) !*! c) == (a !*! (b !*! c))

prop_DistOfMatrix :: Property
prop_DistOfMatrix = SQUAREMATRIX (prop)
 where
  prop :: (Eq (m (m a)), Additive m, Foldable m, Num a)
    => m (m a) -> m (m a) -> m (m a) -> Bool
  prop a b c = (a !*! (b !+! c)) == ((a !*! b) !+! (a !*! c))

prop_DistOfScalar :: Property
prop_DistOfScalar = SQUAREMATRIX (prop)
 where
  prop :: (Eq (m (m a)), Additive m, Foldable m, Num a)
    => a -> m (m a) -> m (m a) -> Bool
  prop a b c = a *!! (b !+! c) == ((a *!! b) !+! (a *!! c))

prop_IdentityNeutralL :: Property
prop_IdentityNeutralL = SQUAREMATRIX (prop)
 where
  prop ::
    ( Eq (m (m a)) , Additive m , Foldable m , Traversable m , Applicative m , Num a)
    =>  m (m a) -> Bool
  prop a = a !*! identity == a

prop_IdentityNeutralR :: Property
prop_IdentityNeutralR = SQUAREMATRIX (prop)
 where
  prop ::
    ( Eq (m (m a)) , Additive m , Foldable m , Traversable m , Applicative m , Num a)
    => m (m a) -> Bool
  prop a = identity !*! a == a

prop_TraceLinear :: Property
prop_TraceLinear = SQUAREMATRIX (prop)
 where
  prop :: (Trace m, Additive m, Num a, Eq a) => m (m a) -> m (m a) -> Bool
  prop a b = trace (a !+! b) == (trace a + trace b)

prop_TraceTranspose :: Property
prop_TraceTranspose = SQUAREMATRIX (prop)
 where
  prop :: (Trace m, Distributive m, Num a, Eq a) => m (m a) -> Bool
  prop a = trace a == trace (transpose a)

prop_TraceSwap :: Property
prop_TraceSwap = SQUAREMATRIX (prop)
 where
  prop :: (Foldable m, Trace m, Additive m, Eq a, Num a)
    => m (m a) -> m (m a) -> Bool
  prop a b = trace (a !*! b) == trace (b !*! a)

prop_dettranspose :: Property
prop_dettranspose = SQUAREMATRIX (prop)
 where
  prop ::
    (Additive m , Distributive m , Fractional a , InvertibleMatrix m , Eq a)
    => m (m a) -> Bool
  prop a = det (transpose a) == det a

prop_detprod :: Property
prop_detprod = SQUAREMATRIX (prop)
 where
  prop :: (Additive m , Foldable m , Fractional a , InvertibleMatrix m , Eq a)
    => m (m a) -> m (m a) -> Bool
  prop a b = det (a !*! b) == det a * det b

prop_detscalarpow :: Property
prop_detscalarpow = SQUAREMATRIX (prop)
 where
  prop :: (Additive m, Fractional a , Foldable m , InvertibleMatrix m , Eq a)
    => m (m a) -> a -> Bool
  prop a c = det (c *!! a) == (c ^ n) * det a
   where
    n = length a

prop_inv :: Property
prop_inv = SQUAREMATRIX (prop)
 where
  prop :: (Additive m, Fractional a, InvertibleMatrix m, Eq (m (m a)), Eq a)
    => m (m a) -> Property
  prop a = (det a /= 0) ==> inv (inv a) == a

prop_invident :: Property
prop_invident = SQUAREMATRIX (prop)
 where
  prop ::
    ( Additive m, Foldable m, Traversable m, Applicative m , Fractional a
    , InvertibleMatrix m , Eq (m (m a)) , Eq a )
    => m (m a) -> Property
  prop a = det a /= 0 ==> a !*! inv a == identity

prop_invmult :: Property
prop_invmult = SQUAREMATRIX (prop)
 where
  prop ::
    ( Additive m, Foldable m, Fractional a, InvertibleMatrix m, Eq a, Eq (m (m a)))
    => m (m a) -> m (m a) -> Property
  prop a b = det a /= 0 && det b /= 0 ==> (inv (a !*! b) == (inv b !*! inv a))

-- TODO A lot of these are in the wrong branch where we only test them on square matrices
tests :: [TestTree]
tests =
  [ testGroup
      "General Matrix Properties" -- These tests don't rely on any specific size of matrix to function
      [ testGroup
          "Basic Properties"
          [ testProperty "Commutativity of !+! A+B=B+A" prop_AddCommut
          , testProperty "Associativity of !+! (A+B)+C=A+(B+C)" prop_AddAssoc
          , testProperty "Distributivity of Matrix A(B+C) = AB+AC" prop_DistOfMatrix
          , testProperty "Distributivity of Scalar a(B+C) = aB+aC" prop_DistOfScalar
          , testProperty "Left and right scalar product are equal Ab=bA" prop_LRScalar
          ]
      , testGroup
          "Transpose Properties"
          [ testProperty "(a^T)^T == a" prop_Transpose
          , testProperty "(A+B)^T == (A^T + B^T)" prop_TransposeDistAdd
          , testProperty "(AB)^T == (B^T A^T)" prop_TransposeDistMul
          ]
      , testGroup
          "Identity Properties"
          [ testProperty "identity is neutral under !*! AI=A" prop_IdentityNeutralR
          , testProperty "identity is neutral under !*! IA=A" prop_IdentityNeutralL
          ]
      , testGroup
          "Trace Properties"
          [ testProperty "trace (A+B) == trace A + trace B" prop_TraceLinear
          , testProperty "trace A == trace (A^T)" prop_TraceTranspose
          , testProperty "trace (AB) == trace (BA)" prop_TraceSwap
          ]
      ]
  , testGroup
      "Square matrix"
      [ testProperty "Associativity of !*! (AB)C=A(BC)" prop_MulAssoc
      , testProperty "inv (inv a) == a" prop_inv
      , testProperty "a !*! inv a == I" prop_invident
      , testProperty "(AB)^-1 == B^-1 * A^-1" prop_invmult
      , testProperty "det A^T = det A" prop_dettranspose
      , testProperty "det (AB) = det A * det B" prop_detprod
      , testProperty "det (cA) = c^2 * det A" prop_detscalarpow
      ]
  ]
