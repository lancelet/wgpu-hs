{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- This file was generated by wgpu-raw-hs-codegen on:
--   2021-08-15T08:56:06.968390
-- Using wgpu-native git hash:
--   b10496e7eed9349f0fd541e6dfe5029cb436de74 wgpu-native (v0.9.2.2)

module WGPU.Raw.Generated.Struct.WGPUDeviceExtras where

#include "wgpu.h"

import Data.Word (Word16, Word32, Word64)
import Data.Int (Int32)
import Foreign
import Foreign.C.Types
import WGPU.Raw.Types
import Prelude (pure, ($!))
import WGPU.Raw.Generated.Enum.WGPUNativeFeature
import WGPU.Raw.Generated.Struct.WGPUChainedStruct

data WGPUDeviceExtras = WGPUDeviceExtras {
  chain :: WGPUChainedStruct,
  maxTextureDimension1D :: Word32,
  maxTextureDimension2D :: Word32,
  maxTextureDimension3D :: Word32,
  maxTextureArrayLayers :: Word32,
  maxBindGroups :: Word32,
  maxDynamicStorageBuffersPerPipelineLayout :: Word32,
  maxStorageBuffersPerShaderStage :: Word32,
  maxStorageBufferBindingSize :: Word32,
  nativeFeatures :: WGPUNativeFeature,
  label :: Ptr (CChar),
  tracePath :: Ptr (CChar)
}

instance Storable WGPUDeviceExtras where
  sizeOf _ = (#size WGPUDeviceExtras)
  alignment = sizeOf
  peek ptr = do
    chain <- (#peek WGPUDeviceExtras, chain) ptr
    maxTextureDimension1D <- (#peek WGPUDeviceExtras, maxTextureDimension1D) ptr
    maxTextureDimension2D <- (#peek WGPUDeviceExtras, maxTextureDimension2D) ptr
    maxTextureDimension3D <- (#peek WGPUDeviceExtras, maxTextureDimension3D) ptr
    maxTextureArrayLayers <- (#peek WGPUDeviceExtras, maxTextureArrayLayers) ptr
    maxBindGroups <- (#peek WGPUDeviceExtras, maxBindGroups) ptr
    maxDynamicStorageBuffersPerPipelineLayout <- (#peek WGPUDeviceExtras, maxDynamicStorageBuffersPerPipelineLayout) ptr
    maxStorageBuffersPerShaderStage <- (#peek WGPUDeviceExtras, maxStorageBuffersPerShaderStage) ptr
    maxStorageBufferBindingSize <- (#peek WGPUDeviceExtras, maxStorageBufferBindingSize) ptr
    nativeFeatures <- (#peek WGPUDeviceExtras, nativeFeatures) ptr
    label <- (#peek WGPUDeviceExtras, label) ptr
    tracePath <- (#peek WGPUDeviceExtras, tracePath) ptr
    pure $! WGPUDeviceExtras{..}
  poke ptr WGPUDeviceExtras{..} = do
    (#poke WGPUDeviceExtras, chain) ptr chain
    (#poke WGPUDeviceExtras, maxTextureDimension1D) ptr maxTextureDimension1D
    (#poke WGPUDeviceExtras, maxTextureDimension2D) ptr maxTextureDimension2D
    (#poke WGPUDeviceExtras, maxTextureDimension3D) ptr maxTextureDimension3D
    (#poke WGPUDeviceExtras, maxTextureArrayLayers) ptr maxTextureArrayLayers
    (#poke WGPUDeviceExtras, maxBindGroups) ptr maxBindGroups
    (#poke WGPUDeviceExtras, maxDynamicStorageBuffersPerPipelineLayout) ptr maxDynamicStorageBuffersPerPipelineLayout
    (#poke WGPUDeviceExtras, maxStorageBuffersPerShaderStage) ptr maxStorageBuffersPerShaderStage
    (#poke WGPUDeviceExtras, maxStorageBufferBindingSize) ptr maxStorageBufferBindingSize
    (#poke WGPUDeviceExtras, nativeFeatures) ptr nativeFeatures
    (#poke WGPUDeviceExtras, label) ptr label
    (#poke WGPUDeviceExtras, tracePath) ptr tracePath