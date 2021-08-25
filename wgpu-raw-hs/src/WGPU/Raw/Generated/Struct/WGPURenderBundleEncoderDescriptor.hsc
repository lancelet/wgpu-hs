{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- This file was generated by wgpu-raw-hs-codegen on:
--   2021-08-25T10:02:03.522705
-- Using wgpu-native git hash:
--   b10496e7eed9349f0fd541e6dfe5029cb436de74 wgpu-native (v0.9.2.2)

module WGPU.Raw.Generated.Struct.WGPURenderBundleEncoderDescriptor where

#include "wgpu.h"

import Data.Word (Word16, Word32, Word64)
import Data.Int (Int32)
import Foreign
import Foreign.C.Types
import WGPU.Raw.Types
import Prelude (pure, ($!))
import WGPU.Raw.Generated.Enum.WGPUTextureFormat
import WGPU.Raw.Generated.Enum.WGPUTextureFormat
import WGPU.Raw.Generated.Struct.WGPUChainedStruct

data WGPURenderBundleEncoderDescriptor = WGPURenderBundleEncoderDescriptor {
  nextInChain :: Ptr (WGPUChainedStruct),
  label :: Ptr (CChar),
  colorFormatsCount :: Word32,
  colorFormats :: Ptr (WGPUTextureFormat),
  depthStencilFormat :: WGPUTextureFormat,
  sampleCount :: Word32
}

instance Storable WGPURenderBundleEncoderDescriptor where
  sizeOf _ = (#size WGPURenderBundleEncoderDescriptor)
  {-# INLINABLE sizeOf #-}
  alignment = sizeOf
  {-# INLINABLE alignment #-}
  peek ptr = do
    nextInChain <- (#peek WGPURenderBundleEncoderDescriptor, nextInChain) ptr
    label <- (#peek WGPURenderBundleEncoderDescriptor, label) ptr
    colorFormatsCount <- (#peek WGPURenderBundleEncoderDescriptor, colorFormatsCount) ptr
    colorFormats <- (#peek WGPURenderBundleEncoderDescriptor, colorFormats) ptr
    depthStencilFormat <- (#peek WGPURenderBundleEncoderDescriptor, depthStencilFormat) ptr
    sampleCount <- (#peek WGPURenderBundleEncoderDescriptor, sampleCount) ptr
    pure $! WGPURenderBundleEncoderDescriptor{..}
  {-# INLINABLE peek #-}
  poke ptr WGPURenderBundleEncoderDescriptor{..} = do
    (#poke WGPURenderBundleEncoderDescriptor, nextInChain) ptr nextInChain
    (#poke WGPURenderBundleEncoderDescriptor, label) ptr label
    (#poke WGPURenderBundleEncoderDescriptor, colorFormatsCount) ptr colorFormatsCount
    (#poke WGPURenderBundleEncoderDescriptor, colorFormats) ptr colorFormats
    (#poke WGPURenderBundleEncoderDescriptor, depthStencilFormat) ptr depthStencilFormat
    (#poke WGPURenderBundleEncoderDescriptor, sampleCount) ptr sampleCount
  {-# INLINABLE poke #-}