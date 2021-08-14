{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- This file was generated by wgpu-raw-hs-codegen on:
--   2021-08-14T11:57:14.367365
-- Using wgpu-native git hash:
--   b10496e7eed9349f0fd541e6dfe5029cb436de74 wgpu-native (v0.9.2.2)

module WGPU.Raw.Generated.Enum.WGPUPrimitiveTopology where

import Data.Word (Word32)
import Foreign (Storable)
import Prelude (Eq, Num, Show)

newtype WGPUPrimitiveTopology = WGPUPrimitiveTopology Word32
  deriving (Eq, Show, Num, Storable)

pattern PointList :: forall a. (Eq a, Num a) => a
pattern PointList = 0x00000000

pattern LineList :: forall a. (Eq a, Num a) => a
pattern LineList = 0x00000001

pattern LineStrip :: forall a. (Eq a, Num a) => a
pattern LineStrip = 0x00000002

pattern TriangleList :: forall a. (Eq a, Num a) => a
pattern TriangleList = 0x00000003

pattern TriangleStrip :: forall a. (Eq a, Num a) => a
pattern TriangleStrip = 0x00000004
