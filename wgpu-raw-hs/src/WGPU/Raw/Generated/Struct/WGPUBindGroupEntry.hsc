{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- This file was generated by wgpu-raw-hs-codegen on:
--   2021-08-15T08:56:06.968390
-- Using wgpu-native git hash:
--   b10496e7eed9349f0fd541e6dfe5029cb436de74 wgpu-native (v0.9.2.2)

module WGPU.Raw.Generated.Struct.WGPUBindGroupEntry where

#include "wgpu.h"

import Data.Word (Word16, Word32, Word64)
import Data.Int (Int32)
import Foreign
import Foreign.C.Types
import WGPU.Raw.Types
import Prelude (pure, ($!))


data WGPUBindGroupEntry = WGPUBindGroupEntry {
  binding :: Word32,
  buffer :: WGPUBuffer,
  offset :: Word64,
  size :: Word64,
  sampler :: WGPUSampler,
  textureView :: WGPUTextureView
}

instance Storable WGPUBindGroupEntry where
  sizeOf _ = (#size WGPUBindGroupEntry)
  alignment = sizeOf
  peek ptr = do
    binding <- (#peek WGPUBindGroupEntry, binding) ptr
    buffer <- (#peek WGPUBindGroupEntry, buffer) ptr
    offset <- (#peek WGPUBindGroupEntry, offset) ptr
    size <- (#peek WGPUBindGroupEntry, size) ptr
    sampler <- (#peek WGPUBindGroupEntry, sampler) ptr
    textureView <- (#peek WGPUBindGroupEntry, textureView) ptr
    pure $! WGPUBindGroupEntry{..}
  poke ptr WGPUBindGroupEntry{..} = do
    (#poke WGPUBindGroupEntry, binding) ptr binding
    (#poke WGPUBindGroupEntry, buffer) ptr buffer
    (#poke WGPUBindGroupEntry, offset) ptr offset
    (#poke WGPUBindGroupEntry, size) ptr size
    (#poke WGPUBindGroupEntry, sampler) ptr sampler
    (#poke WGPUBindGroupEntry, textureView) ptr textureView