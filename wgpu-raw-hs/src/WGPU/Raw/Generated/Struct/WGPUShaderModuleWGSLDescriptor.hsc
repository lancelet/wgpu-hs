{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- This file was generated by wgpu-raw-hs-codegen on:
--   2021-08-25T10:02:03.522705
-- Using wgpu-native git hash:
--   b10496e7eed9349f0fd541e6dfe5029cb436de74 wgpu-native (v0.9.2.2)

module WGPU.Raw.Generated.Struct.WGPUShaderModuleWGSLDescriptor where

#include "wgpu.h"

import Data.Word (Word16, Word32, Word64)
import Data.Int (Int32)
import Foreign
import Foreign.C.Types
import WGPU.Raw.Types
import Prelude (pure, ($!))
import WGPU.Raw.Generated.Struct.WGPUChainedStruct

data WGPUShaderModuleWGSLDescriptor = WGPUShaderModuleWGSLDescriptor {
  chain :: WGPUChainedStruct,
  source :: Ptr (CChar)
}

instance Storable WGPUShaderModuleWGSLDescriptor where
  sizeOf _ = (#size WGPUShaderModuleWGSLDescriptor)
  {-# INLINABLE sizeOf #-}
  alignment = sizeOf
  {-# INLINABLE alignment #-}
  peek ptr = do
    chain <- (#peek WGPUShaderModuleWGSLDescriptor, chain) ptr
    source <- (#peek WGPUShaderModuleWGSLDescriptor, source) ptr
    pure $! WGPUShaderModuleWGSLDescriptor{..}
  {-# INLINABLE peek #-}
  poke ptr WGPUShaderModuleWGSLDescriptor{..} = do
    (#poke WGPUShaderModuleWGSLDescriptor, chain) ptr chain
    (#poke WGPUShaderModuleWGSLDescriptor, source) ptr source
  {-# INLINABLE poke #-}