{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- This file was generated by wgpu-raw-hs-codegen on:
--   2021-08-15T08:56:06.968390
-- Using wgpu-native git hash:
--   b10496e7eed9349f0fd541e6dfe5029cb436de74 wgpu-native (v0.9.2.2)

module WGPU.Raw.Generated.Enum.WGPUQueueWorkDoneStatus where

import Data.Word (Word32)
import Foreign (Storable)
import Prelude (Eq, Num, Show)

newtype WGPUQueueWorkDoneStatus = WGPUQueueWorkDoneStatus Word32
  deriving (Eq, Show, Num, Storable)

pattern Success :: forall a. (Eq a, Num a) => a
pattern Success = 0x00000000

pattern Error :: forall a. (Eq a, Num a) => a
pattern Error = 0x00000001

pattern Unknown :: forall a. (Eq a, Num a) => a
pattern Unknown = 0x00000002

pattern DeviceLost :: forall a. (Eq a, Num a) => a
pattern DeviceLost = 0x00000003
