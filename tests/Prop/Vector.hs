module Prop.Vector (additiveAssoc) where

import Linear.Epsilon (Epsilon, nearZero)
import Linear.Vector (Additive, (^-^))
import Test.QuickCheck (Arbitrary)

additiveAssoc :: (Additive f, Num a, Arbitrary a, Epsilon (f a)) => (f a -> f a -> f a) -> f a -> f a -> f a -> Bool
additiveAssoc op a b c = nearZero ((a `op` b) `op` c ^-^ a `op` (b `op` c))
