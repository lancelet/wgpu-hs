{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- This file was generated by wgpu-raw-hs-codegen on:
--   2021-08-14T08:10:31.492798
-- Using wgpu-native git hash:
--   b10496e7eed9349f0fd541e6dfe5029cb436de74 wgpu-native (v0.9.2.2)

module WGPU.Raw.Generated.Enum.WGPUIndexFormat where

import Data.Word (Word32)
import Foreign (Storable)
import Prelude (Eq, Num, Show)

newtype WGPUIndexFormat = WGPUIndexFormat Word32
  deriving (Eq, Show, Num, Storable)

pattern Undefined :: forall a. (Eq a, Num a) => a
pattern Undefined = 0x00000000

pattern Uint16 :: forall a. (Eq a, Num a) => a
pattern Uint16 = 0x00000001

pattern Uint32 :: forall a. (Eq a, Num a) => a
pattern Uint32 = 0x00000002