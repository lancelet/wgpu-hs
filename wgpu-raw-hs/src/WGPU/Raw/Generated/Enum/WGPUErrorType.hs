{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- This file was generated by wgpu-raw-hs-codegen on:
--   2021-08-25T10:02:03.522705
-- Using wgpu-native git hash:
--   b10496e7eed9349f0fd541e6dfe5029cb436de74 wgpu-native (v0.9.2.2)

module WGPU.Raw.Generated.Enum.WGPUErrorType where

import Data.Word (Word32)
import Foreign (Storable)
import Prelude (Eq, Num, Show)

newtype WGPUErrorType = WGPUErrorType Word32
  deriving (Eq, Show, Num, Storable)

pattern NoError :: forall a. (Eq a, Num a) => a
pattern NoError = 0x00000000

pattern Validation :: forall a. (Eq a, Num a) => a
pattern Validation = 0x00000001

pattern OutOfMemory :: forall a. (Eq a, Num a) => a
pattern OutOfMemory = 0x00000002

pattern Unknown :: forall a. (Eq a, Num a) => a
pattern Unknown = 0x00000003

pattern DeviceLost :: forall a. (Eq a, Num a) => a
pattern DeviceLost = 0x00000004
