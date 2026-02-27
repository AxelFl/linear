module Prop.Vector (additive_assoc) where 

import Linear.Vector (Additive, (^-^))
import Linear.Epsilon (Epsilon, nearZero)
import Test.QuickCheck (Arbitrary)

additive_assoc :: (Additive f, Num a, Arbitrary a, Epsilon (f a)) => (f a -> f a -> f a) -> f a -> f a -> f a -> Bool
additive_assoc op a b c = nearZero ((a `op` b) `op` c ^-^ a `op` (b `op` c))
