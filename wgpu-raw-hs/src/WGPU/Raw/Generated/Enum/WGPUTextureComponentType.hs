{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- This file was generated by wgpu-raw-hs-codegen on:
--   2021-08-24T06:28:07.952342
-- Using wgpu-native git hash:
--   b10496e7eed9349f0fd541e6dfe5029cb436de74 wgpu-native (v0.9.2.2)

module WGPU.Raw.Generated.Enum.WGPUTextureComponentType where

import Data.Word (Word32)
import Foreign (Storable)
import Prelude (Eq, Num, Show)

newtype WGPUTextureComponentType = WGPUTextureComponentType Word32
  deriving (Eq, Show, Num, Storable)

pattern Float :: forall a. (Eq a, Num a) => a
pattern Float = 0x00000000

pattern Sint :: forall a. (Eq a, Num a) => a
pattern Sint = 0x00000001

pattern Uint :: forall a. (Eq a, Num a) => a
pattern Uint = 0x00000002

pattern DepthComparison :: forall a. (Eq a, Num a) => a
pattern DepthComparison = 0x00000003
