{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- This file was generated by wgpu-raw-hs-codegen on:
--   2021-08-25T10:02:03.522705
-- Using wgpu-native git hash:
--   b10496e7eed9349f0fd541e6dfe5029cb436de74 wgpu-native (v0.9.2.2)

module WGPU.Raw.Generated.Enum.WGPUStencilOperation where

import Data.Word (Word32)
import Foreign (Storable)
import Prelude (Eq, Num, Show)

newtype WGPUStencilOperation = WGPUStencilOperation Word32
  deriving (Eq, Show, Num, Storable)

pattern Keep :: forall a. (Eq a, Num a) => a
pattern Keep = 0x00000000

pattern Zero :: forall a. (Eq a, Num a) => a
pattern Zero = 0x00000001

pattern Replace :: forall a. (Eq a, Num a) => a
pattern Replace = 0x00000002

pattern Invert :: forall a. (Eq a, Num a) => a
pattern Invert = 0x00000003

pattern IncrementClamp :: forall a. (Eq a, Num a) => a
pattern IncrementClamp = 0x00000004

pattern DecrementClamp :: forall a. (Eq a, Num a) => a
pattern DecrementClamp = 0x00000005

pattern IncrementWrap :: forall a. (Eq a, Num a) => a
pattern IncrementWrap = 0x00000006

pattern DecrementWrap :: forall a. (Eq a, Num a) => a
pattern DecrementWrap = 0x00000007
