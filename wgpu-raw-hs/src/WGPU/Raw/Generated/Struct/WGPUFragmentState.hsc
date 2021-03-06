{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- This file was generated by wgpu-raw-hs-codegen on:
--   2021-08-25T10:02:03.522705
-- Using wgpu-native git hash:
--   b10496e7eed9349f0fd541e6dfe5029cb436de74 wgpu-native (v0.9.2.2)

module WGPU.Raw.Generated.Struct.WGPUFragmentState where

#include "wgpu.h"

import Data.Word (Word16, Word32, Word64)
import Data.Int (Int32)
import Foreign
import Foreign.C.Types
import WGPU.Raw.Types
import Prelude (pure, ($!))
import WGPU.Raw.Generated.Struct.WGPUChainedStruct
import WGPU.Raw.Generated.Struct.WGPUColorTargetState

data WGPUFragmentState = WGPUFragmentState {
  nextInChain :: Ptr (WGPUChainedStruct),
  shaderModule :: WGPUShaderModule,
  entryPoint :: Ptr (CChar),
  targetCount :: Word32,
  targets :: Ptr (WGPUColorTargetState)
}

instance Storable WGPUFragmentState where
  sizeOf _ = (#size WGPUFragmentState)
  {-# INLINABLE sizeOf #-}
  alignment = sizeOf
  {-# INLINABLE alignment #-}
  peek ptr = do
    nextInChain <- (#peek WGPUFragmentState, nextInChain) ptr
    shaderModule <- (#peek WGPUFragmentState, module) ptr
    entryPoint <- (#peek WGPUFragmentState, entryPoint) ptr
    targetCount <- (#peek WGPUFragmentState, targetCount) ptr
    targets <- (#peek WGPUFragmentState, targets) ptr
    pure $! WGPUFragmentState{..}
  {-# INLINABLE peek #-}
  poke ptr WGPUFragmentState{..} = do
    (#poke WGPUFragmentState, nextInChain) ptr nextInChain
    (#poke WGPUFragmentState, module) ptr shaderModule
    (#poke WGPUFragmentState, entryPoint) ptr entryPoint
    (#poke WGPUFragmentState, targetCount) ptr targetCount
    (#poke WGPUFragmentState, targets) ptr targets
  {-# INLINABLE poke #-}