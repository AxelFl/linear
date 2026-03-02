module Prop.Metric (tests) where

import Linear.Metric (dot, quadrance)
import Linear.V1 (V1(..))
import Linear.V2 (V2(..))
import Linear.V3 (V3(..))
import Linear.V4 (V4(..))
import Prop.V1 ()
import Prop.V2 ()
import Prop.V3 ()
import Prop.V4 ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

prop_dotself :: V3 Rational -> Bool
prop_dotself a =  a `dot` a == quadrance a

testsDotCommut :: TestTree
testsDotCommut = testGroup "Commutativity of scalar product"
  [ testProperty "V1" (prop :: V1 Rational -> V1 Rational -> Bool)
  , testProperty "V2" (prop :: V2 Rational -> V2 Rational -> Bool)
  , testProperty "V3" (prop :: V3 Rational -> V3 Rational -> Bool)
  , testProperty "V4" (prop :: V4 Rational -> V4 Rational -> Bool)
  ]
  where
    prop a b = a `dot` b == b `dot` a

testsDotDist :: TestTree
testsDotDist = testGroup "Distributivity of scalar product over addition"
  [ testProperty "V1" (prop :: V1 Rational -> V1 Rational -> V1 Rational -> Bool)
  , testProperty "V2" (prop :: V2 Rational -> V2 Rational -> V2 Rational -> Bool)
  , testProperty "V3" (prop :: V3 Rational -> V3 Rational -> V3 Rational -> Bool)
  , testProperty "V4" (prop :: V4 Rational -> V4 Rational -> V4 Rational -> Bool)
  ]
  where
    prop a b c = (a+b) `dot` c == (a `dot` c) + (b `dot` c) 

tests :: [TestTree]
tests =
  [ testProperty "dot product is norm squared" prop_dotself
  , testsDotCommut
  , testsDotDist
  ]
