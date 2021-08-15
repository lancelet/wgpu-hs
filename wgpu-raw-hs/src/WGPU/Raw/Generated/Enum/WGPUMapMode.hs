{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- This file was generated by wgpu-raw-hs-codegen on:
--   2021-08-15T08:56:06.968390
-- Using wgpu-native git hash:
--   b10496e7eed9349f0fd541e6dfe5029cb436de74 wgpu-native (v0.9.2.2)

module WGPU.Raw.Generated.Enum.WGPUMapMode where

import Data.Word (Word32)
import Foreign (Storable)
import Prelude (Eq, Num, Show)

newtype WGPUMapMode = WGPUMapMode Word32
  deriving (Eq, Show, Num, Storable)

pattern Read :: forall a. (Eq a, Num a) => a
pattern Read = 0x00000001

pattern Write :: forall a. (Eq a, Num a) => a
pattern Write = 0x00000002
