{-# OPTIONS_GHC -Wno-orphans #-}
module Prop.V4 () where

import Linear.V4 (V4(..))
import Test.QuickCheck (Arbitrary(..))

instance Arbitrary a => Arbitrary (V4 a) where
  arbitrary = V4 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
