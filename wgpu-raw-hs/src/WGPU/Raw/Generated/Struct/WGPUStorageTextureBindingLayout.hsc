{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- This file was generated by wgpu-raw-hs-codegen on:
--   2021-08-14T08:10:31.492798
-- Using wgpu-native git hash:
--   b10496e7eed9349f0fd541e6dfe5029cb436de74 wgpu-native (v0.9.2.2)

module WGPU.Raw.Generated.Struct.WGPUStorageTextureBindingLayout where

#include "wgpu.h"

import Data.Word (Word16, Word32, Word64)
import Data.Int (Int32)
import Foreign
import Foreign.C.Types
import WGPU.Raw.Types
import Prelude (pure, ($!))
import WGPU.Raw.Generated.Enum.WGPUStorageTextureAccess
import WGPU.Raw.Generated.Enum.WGPUTextureFormat
import WGPU.Raw.Generated.Enum.WGPUTextureViewDimension
import WGPU.Raw.Generated.Struct.WGPUChainedStruct

data WGPUStorageTextureBindingLayout = WGPUStorageTextureBindingLayout {
  nextInChain :: Ptr (WGPUChainedStruct),
  access :: WGPUStorageTextureAccess,
  format :: WGPUTextureFormat,
  viewDimension :: WGPUTextureViewDimension
}

instance Storable WGPUStorageTextureBindingLayout where
  sizeOf _ = (#size WGPUStorageTextureBindingLayout)
  alignment = sizeOf
  peek ptr = do
    nextInChain <- (#peek WGPUStorageTextureBindingLayout, nextInChain) ptr
    access <- (#peek WGPUStorageTextureBindingLayout, access) ptr
    format <- (#peek WGPUStorageTextureBindingLayout, format) ptr
    viewDimension <- (#peek WGPUStorageTextureBindingLayout, viewDimension) ptr
    pure $! WGPUStorageTextureBindingLayout{..}
  poke ptr WGPUStorageTextureBindingLayout{..} = do
    (#poke WGPUStorageTextureBindingLayout, nextInChain) ptr nextInChain
    (#poke WGPUStorageTextureBindingLayout, access) ptr access
    (#poke WGPUStorageTextureBindingLayout, format) ptr format
    (#poke WGPUStorageTextureBindingLayout, viewDimension) ptr viewDimension