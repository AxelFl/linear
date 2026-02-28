module Prop.Matrix (tests) where

import Linear.Epsilon (nearZero)
import Linear.Matrix (M22, det22, inv22, M33, transpose, (!*!), (!+!), (!-!), identity)
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

prop_m22invmult :: M22 Double -> M22 Double -> Property
prop_m22invmult a b = (det22 a /= 0 && det22 b /= 0) ==> nearZero $ inv22 (a !*! b) !-! (inv22 b !*! inv22 a)

prop_m33multident :: M33 Double -> Bool
prop_m33multident a = a !*! identity == a

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
      ]
  , testGroup
      "2x2 matrix"
      [ testProperty "inv22 (inv22 a) == a" prop_m22inv
      , testProperty "(AB)^-1 == B^-1 * A^-1" prop_m22invmult
      ]
  ]
