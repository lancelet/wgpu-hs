{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- This file was generated by wgpu-raw-hs-codegen on:
--   2021-08-24T06:28:07.952342
-- Using wgpu-native git hash:
--   b10496e7eed9349f0fd541e6dfe5029cb436de74 wgpu-native (v0.9.2.2)

module WGPU.Raw.Generated.Struct.WGPUTextureBindingLayout where

#include "wgpu.h"

import Data.Word (Word16, Word32, Word64)
import Data.Int (Int32)
import Foreign
import Foreign.C.Types
import WGPU.Raw.Types
import Prelude (pure, ($!))
import WGPU.Raw.Generated.Enum.WGPUTextureSampleType
import WGPU.Raw.Generated.Enum.WGPUTextureViewDimension
import WGPU.Raw.Generated.Struct.WGPUChainedStruct

data WGPUTextureBindingLayout = WGPUTextureBindingLayout {
  nextInChain :: Ptr (WGPUChainedStruct),
  sampleType :: WGPUTextureSampleType,
  viewDimension :: WGPUTextureViewDimension,
  multisampled :: CBool
}

instance Storable WGPUTextureBindingLayout where
  sizeOf _ = (#size WGPUTextureBindingLayout)
  alignment = sizeOf
  peek ptr = do
    nextInChain <- (#peek WGPUTextureBindingLayout, nextInChain) ptr
    sampleType <- (#peek WGPUTextureBindingLayout, sampleType) ptr
    viewDimension <- (#peek WGPUTextureBindingLayout, viewDimension) ptr
    multisampled <- (#peek WGPUTextureBindingLayout, multisampled) ptr
    pure $! WGPUTextureBindingLayout{..}
  poke ptr WGPUTextureBindingLayout{..} = do
    (#poke WGPUTextureBindingLayout, nextInChain) ptr nextInChain
    (#poke WGPUTextureBindingLayout, sampleType) ptr sampleType
    (#poke WGPUTextureBindingLayout, viewDimension) ptr viewDimension
    (#poke WGPUTextureBindingLayout, multisampled) ptr multisampled