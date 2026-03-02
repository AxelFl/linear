module Prop.Vector (tests) where

import Linear.V1 (V1)
import Linear.V2 (V2)
import Linear.V3 (V3)
import Linear.V4 (V4)
import Linear.Vector (negated, zero, (*^), (^*), (^+^))
import Prop.V1 ()
import Prop.V2 ()
import Prop.V3 ()
import Prop.V4 ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

testsAddAssoc :: TestTree
testsAddAssoc = testGroup "associativity of ^+^"
  [ testProperty "V1" (prop_addassoc :: V1 Rational -> V1 Rational -> V1 Rational -> Bool)
  , testProperty "V2" (prop_addassoc :: V2 Rational -> V2 Rational -> V2 Rational -> Bool)
  , testProperty "V3" (prop_addassoc :: V3 Rational -> V3 Rational -> V3 Rational -> Bool)
  , testProperty "V4" (prop_addassoc :: V4 Rational -> V4 Rational -> V4 Rational -> Bool)
  ]
  where
      prop_addassoc a b c = ((a ^+^ b) ^+^ c) == (a ^+^ (b ^+^ c))

testsAddCommut :: TestTree
testsAddCommut = testGroup "commutativity of ^+^"
  [ testProperty "V1" (prop_addcommut :: V1 Rational -> V1 Rational -> Bool)
  , testProperty "V2" (prop_addcommut :: V2 Rational -> V2 Rational -> Bool)
  , testProperty "V3" (prop_addcommut :: V3 Rational -> V3 Rational -> Bool)
  , testProperty "V4" (prop_addcommut :: V4 Rational -> V4 Rational -> Bool)
  ]
  where
    prop_addcommut a b = (a ^+^ b) == (b ^+^ a)

testsLRScalarProduct :: TestTree
testsLRScalarProduct = testGroup "Left and right scalar product are equal"
  [ testProperty "V1" (prop :: V1 Rational -> Rational -> Bool)
  , testProperty "V2" (prop :: V2 Rational -> Rational -> Bool)
  , testProperty "V3" (prop :: V3 Rational -> Rational -> Bool)
  , testProperty "V4" (prop :: V4 Rational -> Rational -> Bool)
  ]
  where
    prop v a = v ^* a == a *^ v

testsDistScalarR :: TestTree
testsDistScalarR = testGroup "Right scalar are distributive over vector addition"
  [ testProperty "V1" (prop :: V1 Rational -> V1 Rational -> Rational -> Bool)
  , testProperty "V2" (prop :: V2 Rational -> V2 Rational -> Rational -> Bool)
  , testProperty "V3" (prop :: V3 Rational -> V3 Rational -> Rational -> Bool)
  , testProperty "V4" (prop :: V4 Rational -> V4 Rational -> Rational -> Bool)
  ]
  where
    prop a b c = (a ^+^ b) ^* c == (a^*c) ^+^ (b^*c)

testsDistScalarL :: TestTree
testsDistScalarL = testGroup "Left scalar are distributive over vector addition"
  [ testProperty "V1" (prop :: V1 Rational -> V1 Rational -> Rational -> Bool)
  , testProperty "V2" (prop :: V2 Rational -> V2 Rational -> Rational -> Bool)
  , testProperty "V3" (prop :: V3 Rational -> V3 Rational -> Rational -> Bool)
  , testProperty "V4" (prop :: V4 Rational -> V4 Rational -> Rational -> Bool)
  ]
  where
    prop a b c = c *^ (a ^+^ b) == (c*^a) ^+^ (c*^b)

testsNegateVector :: TestTree
testsNegateVector = testGroup "Negation is inverse under ^+^"
  [ testProperty "V1" (prop :: V1 Rational -> Bool)
  , testProperty "V2" (prop :: V2 Rational -> Bool)
  , testProperty "V3" (prop :: V3 Rational -> Bool)
  , testProperty "V4" (prop :: V4 Rational -> Bool)
  ]
  where
    prop a = a ^+^ negated a == zero

tests :: [TestTree]
tests =
  [ testsLRScalarProduct
  , testsDistScalarR
  , testsDistScalarL
  , testsNegateVector
  , testsAddAssoc
  , testsAddCommut
  ]
