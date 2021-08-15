{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- This file was generated by wgpu-raw-hs-codegen on:
--   2021-08-15T08:56:06.968390
-- Using wgpu-native git hash:
--   b10496e7eed9349f0fd541e6dfe5029cb436de74 wgpu-native (v0.9.2.2)

module WGPU.Raw.Generated.Struct.WGPUVertexAttribute where

#include "wgpu.h"

import Data.Word (Word16, Word32, Word64)
import Data.Int (Int32)
import Foreign
import Foreign.C.Types
import WGPU.Raw.Types
import Prelude (pure, ($!))
import WGPU.Raw.Generated.Enum.WGPUVertexFormat

data WGPUVertexAttribute = WGPUVertexAttribute {
  format :: WGPUVertexFormat,
  offset :: Word64,
  shaderLocation :: Word32
}

instance Storable WGPUVertexAttribute where
  sizeOf _ = (#size WGPUVertexAttribute)
  alignment = sizeOf
  peek ptr = do
    format <- (#peek WGPUVertexAttribute, format) ptr
    offset <- (#peek WGPUVertexAttribute, offset) ptr
    shaderLocation <- (#peek WGPUVertexAttribute, shaderLocation) ptr
    pure $! WGPUVertexAttribute{..}
  poke ptr WGPUVertexAttribute{..} = do
    (#poke WGPUVertexAttribute, format) ptr format
    (#poke WGPUVertexAttribute, offset) ptr offset
    (#poke WGPUVertexAttribute, shaderLocation) ptr shaderLocation