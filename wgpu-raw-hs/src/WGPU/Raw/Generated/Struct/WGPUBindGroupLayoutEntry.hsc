{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- This file was generated by wgpu-raw-hs-codegen on:
--   2021-08-25T10:02:03.522705
-- Using wgpu-native git hash:
--   b10496e7eed9349f0fd541e6dfe5029cb436de74 wgpu-native (v0.9.2.2)

module WGPU.Raw.Generated.Struct.WGPUBindGroupLayoutEntry where

#include "wgpu.h"

import Data.Word (Word16, Word32, Word64)
import Data.Int (Int32)
import Foreign
import Foreign.C.Types
import WGPU.Raw.Types
import Prelude (pure, ($!))
import WGPU.Raw.Generated.Struct.WGPUChainedStruct
import WGPU.Raw.Generated.Struct.WGPUBufferBindingLayout
import WGPU.Raw.Generated.Struct.WGPUSamplerBindingLayout
import WGPU.Raw.Generated.Struct.WGPUTextureBindingLayout
import WGPU.Raw.Generated.Struct.WGPUStorageTextureBindingLayout

data WGPUBindGroupLayoutEntry = WGPUBindGroupLayoutEntry {
  nextInChain :: Ptr (WGPUChainedStruct),
  binding :: Word32,
  visibility :: WGPUShaderStageFlags,
  buffer :: WGPUBufferBindingLayout,
  sampler :: WGPUSamplerBindingLayout,
  texture :: WGPUTextureBindingLayout,
  storageTexture :: WGPUStorageTextureBindingLayout
}

instance Storable WGPUBindGroupLayoutEntry where
  sizeOf _ = (#size WGPUBindGroupLayoutEntry)
  {-# INLINABLE sizeOf #-}
  alignment = sizeOf
  {-# INLINABLE alignment #-}
  peek ptr = do
    nextInChain <- (#peek WGPUBindGroupLayoutEntry, nextInChain) ptr
    binding <- (#peek WGPUBindGroupLayoutEntry, binding) ptr
    visibility <- (#peek WGPUBindGroupLayoutEntry, visibility) ptr
    buffer <- (#peek WGPUBindGroupLayoutEntry, buffer) ptr
    sampler <- (#peek WGPUBindGroupLayoutEntry, sampler) ptr
    texture <- (#peek WGPUBindGroupLayoutEntry, texture) ptr
    storageTexture <- (#peek WGPUBindGroupLayoutEntry, storageTexture) ptr
    pure $! WGPUBindGroupLayoutEntry{..}
  {-# INLINABLE peek #-}
  poke ptr WGPUBindGroupLayoutEntry{..} = do
    (#poke WGPUBindGroupLayoutEntry, nextInChain) ptr nextInChain
    (#poke WGPUBindGroupLayoutEntry, binding) ptr binding
    (#poke WGPUBindGroupLayoutEntry, visibility) ptr visibility
    (#poke WGPUBindGroupLayoutEntry, buffer) ptr buffer
    (#poke WGPUBindGroupLayoutEntry, sampler) ptr sampler
    (#poke WGPUBindGroupLayoutEntry, texture) ptr texture
    (#poke WGPUBindGroupLayoutEntry, storageTexture) ptr storageTexture
  {-# INLINABLE poke #-}