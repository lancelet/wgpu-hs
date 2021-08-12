{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- This file was generated by wgpu-raw-hs-codegen on:
--   2021-08-14T08:10:31.492798
-- Using wgpu-native git hash:
--   b10496e7eed9349f0fd541e6dfe5029cb436de74 wgpu-native (v0.9.2.2)

module WGPU.Raw.Generated.Struct.WGPUSwapChainDescriptor where

#include "wgpu.h"

import Data.Word (Word16, Word32, Word64)
import Data.Int (Int32)
import Foreign
import Foreign.C.Types
import WGPU.Raw.Types
import Prelude (pure, ($!))
import WGPU.Raw.Generated.Enum.WGPUTextureFormat
import WGPU.Raw.Generated.Enum.WGPUPresentMode
import WGPU.Raw.Generated.Struct.WGPUChainedStruct

data WGPUSwapChainDescriptor = WGPUSwapChainDescriptor {
  nextInChain :: Ptr (WGPUChainedStruct),
  label :: Ptr (CChar),
  usage :: WGPUTextureUsageFlags,
  format :: WGPUTextureFormat,
  width :: Word32,
  height :: Word32,
  presentMode :: WGPUPresentMode
}

instance Storable WGPUSwapChainDescriptor where
  sizeOf _ = (#size WGPUSwapChainDescriptor)
  alignment = sizeOf
  peek ptr = do
    nextInChain <- (#peek WGPUSwapChainDescriptor, nextInChain) ptr
    label <- (#peek WGPUSwapChainDescriptor, label) ptr
    usage <- (#peek WGPUSwapChainDescriptor, usage) ptr
    format <- (#peek WGPUSwapChainDescriptor, format) ptr
    width <- (#peek WGPUSwapChainDescriptor, width) ptr
    height <- (#peek WGPUSwapChainDescriptor, height) ptr
    presentMode <- (#peek WGPUSwapChainDescriptor, presentMode) ptr
    pure $! WGPUSwapChainDescriptor{..}
  poke ptr WGPUSwapChainDescriptor{..} = do
    (#poke WGPUSwapChainDescriptor, nextInChain) ptr nextInChain
    (#poke WGPUSwapChainDescriptor, label) ptr label
    (#poke WGPUSwapChainDescriptor, usage) ptr usage
    (#poke WGPUSwapChainDescriptor, format) ptr format
    (#poke WGPUSwapChainDescriptor, width) ptr width
    (#poke WGPUSwapChainDescriptor, height) ptr height
    (#poke WGPUSwapChainDescriptor, presentMode) ptr presentMode