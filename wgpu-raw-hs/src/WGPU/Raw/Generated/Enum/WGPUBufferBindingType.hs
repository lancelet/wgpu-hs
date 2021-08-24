{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- This file was generated by wgpu-raw-hs-codegen on:
--   2021-08-24T06:28:07.952342
-- Using wgpu-native git hash:
--   b10496e7eed9349f0fd541e6dfe5029cb436de74 wgpu-native (v0.9.2.2)

module WGPU.Raw.Generated.Enum.WGPUBufferBindingType where

import Data.Word (Word32)
import Foreign (Storable)
import Prelude (Eq, Num, Show)

newtype WGPUBufferBindingType = WGPUBufferBindingType Word32
  deriving (Eq, Show, Num, Storable)

pattern Undefined :: forall a. (Eq a, Num a) => a
pattern Undefined = 0x00000000

pattern Uniform :: forall a. (Eq a, Num a) => a
pattern Uniform = 0x00000001

pattern Storage :: forall a. (Eq a, Num a) => a
pattern Storage = 0x00000002

pattern ReadOnlyStorage :: forall a. (Eq a, Num a) => a
pattern ReadOnlyStorage = 0x00000003
