module Prop.Metric (tests) where

import Linear.Metric (dot, quadrance)
import Linear.V3 (V3(..))
import Prop.V3 ()
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)

prop_dotself :: V3 Rational -> Bool
prop_dotself a =  a `dot` a == quadrance a

tests :: [TestTree]
tests =
  [ testProperty "dot product is norm squared" prop_dotself
  ]
