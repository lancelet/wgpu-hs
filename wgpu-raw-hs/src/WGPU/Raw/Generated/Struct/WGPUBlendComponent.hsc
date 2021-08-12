{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- This file was generated by wgpu-raw-hs-codegen on:
--   2021-08-14T08:10:31.492798
-- Using wgpu-native git hash:
--   b10496e7eed9349f0fd541e6dfe5029cb436de74 wgpu-native (v0.9.2.2)

module WGPU.Raw.Generated.Struct.WGPUBlendComponent where

#include "wgpu.h"

import Data.Word (Word16, Word32, Word64)
import Data.Int (Int32)
import Foreign
import Foreign.C.Types
import WGPU.Raw.Types
import Prelude (pure, ($!))
import WGPU.Raw.Generated.Enum.WGPUBlendFactor
import WGPU.Raw.Generated.Enum.WGPUBlendFactor
import WGPU.Raw.Generated.Enum.WGPUBlendOperation

data WGPUBlendComponent = WGPUBlendComponent {
  srcFactor :: WGPUBlendFactor,
  dstFactor :: WGPUBlendFactor,
  operation :: WGPUBlendOperation
}

instance Storable WGPUBlendComponent where
  sizeOf _ = (#size WGPUBlendComponent)
  alignment = sizeOf
  peek ptr = do
    srcFactor <- (#peek WGPUBlendComponent, srcFactor) ptr
    dstFactor <- (#peek WGPUBlendComponent, dstFactor) ptr
    operation <- (#peek WGPUBlendComponent, operation) ptr
    pure $! WGPUBlendComponent{..}
  poke ptr WGPUBlendComponent{..} = do
    (#poke WGPUBlendComponent, srcFactor) ptr srcFactor
    (#poke WGPUBlendComponent, dstFactor) ptr dstFactor
    (#poke WGPUBlendComponent, operation) ptr operation