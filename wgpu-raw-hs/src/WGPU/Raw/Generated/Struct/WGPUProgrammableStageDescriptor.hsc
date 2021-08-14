{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- This file was generated by wgpu-raw-hs-codegen on:
--   2021-08-14T11:57:14.367365
-- Using wgpu-native git hash:
--   b10496e7eed9349f0fd541e6dfe5029cb436de74 wgpu-native (v0.9.2.2)

module WGPU.Raw.Generated.Struct.WGPUProgrammableStageDescriptor where

#include "wgpu.h"

import Data.Word (Word16, Word32, Word64)
import Data.Int (Int32)
import Foreign
import Foreign.C.Types
import WGPU.Raw.Types
import Prelude (pure, ($!))
import WGPU.Raw.Generated.Struct.WGPUChainedStruct

data WGPUProgrammableStageDescriptor = WGPUProgrammableStageDescriptor {
  nextInChain :: Ptr (WGPUChainedStruct),
  shaderModule :: WGPUShaderModule,
  entryPoint :: Ptr (CChar)
}

instance Storable WGPUProgrammableStageDescriptor where
  sizeOf _ = (#size WGPUProgrammableStageDescriptor)
  alignment = sizeOf
  peek ptr = do
    nextInChain <- (#peek WGPUProgrammableStageDescriptor, nextInChain) ptr
    shaderModule <- (#peek WGPUProgrammableStageDescriptor, module) ptr
    entryPoint <- (#peek WGPUProgrammableStageDescriptor, entryPoint) ptr
    pure $! WGPUProgrammableStageDescriptor{..}
  poke ptr WGPUProgrammableStageDescriptor{..} = do
    (#poke WGPUProgrammableStageDescriptor, nextInChain) ptr nextInChain
    (#poke WGPUProgrammableStageDescriptor, module) ptr shaderModule
    (#poke WGPUProgrammableStageDescriptor, entryPoint) ptr entryPoint