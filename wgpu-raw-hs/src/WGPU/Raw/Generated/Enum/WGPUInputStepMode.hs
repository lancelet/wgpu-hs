{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- This file was generated by wgpu-raw-hs-codegen on:
--   2021-08-24T06:28:07.952342
-- Using wgpu-native git hash:
--   b10496e7eed9349f0fd541e6dfe5029cb436de74 wgpu-native (v0.9.2.2)

module WGPU.Raw.Generated.Enum.WGPUInputStepMode where

import Data.Word (Word32)
import Foreign (Storable)
import Prelude (Eq, Num, Show)

newtype WGPUInputStepMode = WGPUInputStepMode Word32
  deriving (Eq, Show, Num, Storable)

pattern Vertex :: forall a. (Eq a, Num a) => a
pattern Vertex = 0x00000000

pattern Instance :: forall a. (Eq a, Num a) => a
pattern Instance = 0x00000001
