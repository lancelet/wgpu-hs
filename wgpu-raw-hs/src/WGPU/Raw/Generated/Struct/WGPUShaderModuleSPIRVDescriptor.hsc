{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- This file was generated by wgpu-raw-hs-codegen on:
--   2021-08-14T08:10:31.492798
-- Using wgpu-native git hash:
--   b10496e7eed9349f0fd541e6dfe5029cb436de74 wgpu-native (v0.9.2.2)

module WGPU.Raw.Generated.Struct.WGPUShaderModuleSPIRVDescriptor where

#include "wgpu.h"

import Data.Word (Word16, Word32, Word64)
import Data.Int (Int32)
import Foreign
import Foreign.C.Types
import WGPU.Raw.Types
import Prelude (pure, ($!))
import WGPU.Raw.Generated.Struct.WGPUChainedStruct

data WGPUShaderModuleSPIRVDescriptor = WGPUShaderModuleSPIRVDescriptor {
  chain :: WGPUChainedStruct,
  codeSize :: Word32,
  code :: Ptr (Word32)
}

instance Storable WGPUShaderModuleSPIRVDescriptor where
  sizeOf _ = (#size WGPUShaderModuleSPIRVDescriptor)
  alignment = sizeOf
  peek ptr = do
    chain <- (#peek WGPUShaderModuleSPIRVDescriptor, chain) ptr
    codeSize <- (#peek WGPUShaderModuleSPIRVDescriptor, codeSize) ptr
    code <- (#peek WGPUShaderModuleSPIRVDescriptor, code) ptr
    pure $! WGPUShaderModuleSPIRVDescriptor{..}
  poke ptr WGPUShaderModuleSPIRVDescriptor{..} = do
    (#poke WGPUShaderModuleSPIRVDescriptor, chain) ptr chain
    (#poke WGPUShaderModuleSPIRVDescriptor, codeSize) ptr codeSize
    (#poke WGPUShaderModuleSPIRVDescriptor, code) ptr code