{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- This file was generated by wgpu-raw-hs-codegen on:
--   2021-08-14T11:57:14.367365
-- Using wgpu-native git hash:
--   b10496e7eed9349f0fd541e6dfe5029cb436de74 wgpu-native (v0.9.2.2)

module WGPU.Raw.Generated.Enum.WGPUCreatePipelineAsyncStatus where

import Data.Word (Word32)
import Foreign (Storable)
import Prelude (Eq, Num, Show)

newtype WGPUCreatePipelineAsyncStatus = WGPUCreatePipelineAsyncStatus Word32
  deriving (Eq, Show, Num, Storable)

pattern Success :: forall a. (Eq a, Num a) => a
pattern Success = 0x00000000

pattern Error :: forall a. (Eq a, Num a) => a
pattern Error = 0x00000001

pattern DeviceLost :: forall a. (Eq a, Num a) => a
pattern DeviceLost = 0x00000002

pattern DeviceDestroyed :: forall a. (Eq a, Num a) => a
pattern DeviceDestroyed = 0x00000003

pattern Unknown :: forall a. (Eq a, Num a) => a
pattern Unknown = 0x00000004
