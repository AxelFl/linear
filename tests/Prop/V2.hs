{-# OPTIONS_GHC -Wno-orphans #-}
module Prop.V2 (arbitrary) where

import Linear.V2 (V2(..))
import Test.QuickCheck (Arbitrary(..))

instance Arbitrary a => Arbitrary (V2 a) where
  arbitrary = V2 <$> arbitrary <*> arbitrary
