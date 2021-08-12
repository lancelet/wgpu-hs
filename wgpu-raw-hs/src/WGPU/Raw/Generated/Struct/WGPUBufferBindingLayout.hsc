{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- This file was generated by wgpu-raw-hs-codegen on:
--   2021-08-14T08:10:31.492798
-- Using wgpu-native git hash:
--   b10496e7eed9349f0fd541e6dfe5029cb436de74 wgpu-native (v0.9.2.2)

module WGPU.Raw.Generated.Struct.WGPUBufferBindingLayout where

#include "wgpu.h"

import Data.Word (Word16, Word32, Word64)
import Data.Int (Int32)
import Foreign
import Foreign.C.Types
import WGPU.Raw.Types
import Prelude (pure, ($!))
import WGPU.Raw.Generated.Enum.WGPUBufferBindingType
import WGPU.Raw.Generated.Struct.WGPUChainedStruct

data WGPUBufferBindingLayout = WGPUBufferBindingLayout {
  nextInChain :: Ptr (WGPUChainedStruct),
  typ :: WGPUBufferBindingType,
  hasDynamicOffset :: CBool,
  minBindingSize :: Word64
}

instance Storable WGPUBufferBindingLayout where
  sizeOf _ = (#size WGPUBufferBindingLayout)
  alignment = sizeOf
  peek ptr = do
    nextInChain <- (#peek WGPUBufferBindingLayout, nextInChain) ptr
    typ <- (#peek WGPUBufferBindingLayout, type) ptr
    hasDynamicOffset <- (#peek WGPUBufferBindingLayout, hasDynamicOffset) ptr
    minBindingSize <- (#peek WGPUBufferBindingLayout, minBindingSize) ptr
    pure $! WGPUBufferBindingLayout{..}
  poke ptr WGPUBufferBindingLayout{..} = do
    (#poke WGPUBufferBindingLayout, nextInChain) ptr nextInChain
    (#poke WGPUBufferBindingLayout, type) ptr typ
    (#poke WGPUBufferBindingLayout, hasDynamicOffset) ptr hasDynamicOffset
    (#poke WGPUBufferBindingLayout, minBindingSize) ptr minBindingSize