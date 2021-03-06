{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- This file was generated by wgpu-raw-hs-codegen on:
--   2021-08-25T10:02:03.522705
-- Using wgpu-native git hash:
--   b10496e7eed9349f0fd541e6dfe5029cb436de74 wgpu-native (v0.9.2.2)

module WGPU.Raw.Generated.Struct.WGPUTextureViewDescriptor where

#include "wgpu.h"

import Data.Word (Word16, Word32, Word64)
import Data.Int (Int32)
import Foreign
import Foreign.C.Types
import WGPU.Raw.Types
import Prelude (pure, ($!))
import WGPU.Raw.Generated.Enum.WGPUTextureFormat
import WGPU.Raw.Generated.Enum.WGPUTextureViewDimension
import WGPU.Raw.Generated.Enum.WGPUTextureAspect
import WGPU.Raw.Generated.Struct.WGPUChainedStruct

data WGPUTextureViewDescriptor = WGPUTextureViewDescriptor {
  nextInChain :: Ptr (WGPUChainedStruct),
  label :: Ptr (CChar),
  format :: WGPUTextureFormat,
  dimension :: WGPUTextureViewDimension,
  baseMipLevel :: Word32,
  mipLevelCount :: Word32,
  baseArrayLayer :: Word32,
  arrayLayerCount :: Word32,
  aspect :: WGPUTextureAspect
}

instance Storable WGPUTextureViewDescriptor where
  sizeOf _ = (#size WGPUTextureViewDescriptor)
  {-# INLINABLE sizeOf #-}
  alignment = sizeOf
  {-# INLINABLE alignment #-}
  peek ptr = do
    nextInChain <- (#peek WGPUTextureViewDescriptor, nextInChain) ptr
    label <- (#peek WGPUTextureViewDescriptor, label) ptr
    format <- (#peek WGPUTextureViewDescriptor, format) ptr
    dimension <- (#peek WGPUTextureViewDescriptor, dimension) ptr
    baseMipLevel <- (#peek WGPUTextureViewDescriptor, baseMipLevel) ptr
    mipLevelCount <- (#peek WGPUTextureViewDescriptor, mipLevelCount) ptr
    baseArrayLayer <- (#peek WGPUTextureViewDescriptor, baseArrayLayer) ptr
    arrayLayerCount <- (#peek WGPUTextureViewDescriptor, arrayLayerCount) ptr
    aspect <- (#peek WGPUTextureViewDescriptor, aspect) ptr
    pure $! WGPUTextureViewDescriptor{..}
  {-# INLINABLE peek #-}
  poke ptr WGPUTextureViewDescriptor{..} = do
    (#poke WGPUTextureViewDescriptor, nextInChain) ptr nextInChain
    (#poke WGPUTextureViewDescriptor, label) ptr label
    (#poke WGPUTextureViewDescriptor, format) ptr format
    (#poke WGPUTextureViewDescriptor, dimension) ptr dimension
    (#poke WGPUTextureViewDescriptor, baseMipLevel) ptr baseMipLevel
    (#poke WGPUTextureViewDescriptor, mipLevelCount) ptr mipLevelCount
    (#poke WGPUTextureViewDescriptor, baseArrayLayer) ptr baseArrayLayer
    (#poke WGPUTextureViewDescriptor, arrayLayerCount) ptr arrayLayerCount
    (#poke WGPUTextureViewDescriptor, aspect) ptr aspect
  {-# INLINABLE poke #-}