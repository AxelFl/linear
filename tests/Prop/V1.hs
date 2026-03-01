{-# OPTIONS_GHC -Wno-orphans #-}

module Prop.V1 () where

import Linear.V1 (V1 (..))
import Test.QuickCheck (Arbitrary (..))

instance (Arbitrary a) => Arbitrary (V1 a) where
  arbitrary = V1 <$> arbitrary
