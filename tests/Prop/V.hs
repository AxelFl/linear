{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Prop.V (prop1_V, prop2_V, prop3_V) where

import Data.Proxy (Proxy (..))
import qualified Data.Vector as V
import Linear.V (Dim (..), V (..))
import Test.QuickCheck (Arbitrary (..), Property, Testable, getNonNegative, property, vector)

import GHC.TypeLits (Nat, KnownNat, SomeNat (..), someNatVal)

instance (Dim n, Arbitrary a) => Arbitrary (V n a) where
  arbitrary = do
    l <- vector $ reflectDim $ Proxy @n
    pure $ V $ V.fromList l

instance Arbitrary SomeNat where
  arbitrary = do
    nat <- getNonNegative <$> arbitrary
    case someNatVal nat of
      Just sn -> pure sn
      Nothing -> error "impossible: `nat` should be non-negative"

prop1_V ::
  forall a prop.
  (Arbitrary a, Show a, Testable prop) =>
  (forall (n :: Nat). KnownNat n => V n a -> prop) ->
  SomeNat ->
  Property
prop1_V f (SomeNat (Proxy :: Proxy n)) = property $ f @n

prop2_V ::
  forall a prop.
  (Arbitrary a, Show a, Testable prop) =>
  (forall (n :: Nat). KnownNat n => V n a -> V n a -> prop) ->
  SomeNat ->
  Property
prop2_V f (SomeNat (Proxy :: Proxy n)) = property $ f @n

prop3_V ::
  forall a prop.
  (Arbitrary a, Show a, Testable prop) =>
  (forall (n :: Nat). KnownNat n => V n a -> V n a -> V n a -> prop) ->
  SomeNat ->
  Property
prop3_V f (SomeNat (Proxy :: Proxy n)) = property $ f @n
