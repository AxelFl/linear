{-# OPTIONS_GHC -Wno-orphans #-}

module Prop.V0 () where

import Linear.V0 (V0 (..))
import Test.QuickCheck (Arbitrary (..))

instance Arbitrary (V0 a) where
  arbitrary = V0
