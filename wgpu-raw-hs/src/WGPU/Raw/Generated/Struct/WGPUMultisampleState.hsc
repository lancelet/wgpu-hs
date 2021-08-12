{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- This file was generated by wgpu-raw-hs-codegen on:
--   2021-08-14T08:10:31.492798
-- Using wgpu-native git hash:
--   b10496e7eed9349f0fd541e6dfe5029cb436de74 wgpu-native (v0.9.2.2)

module WGPU.Raw.Generated.Struct.WGPUMultisampleState where

#include "wgpu.h"

import Data.Word (Word16, Word32, Word64)
import Data.Int (Int32)
import Foreign
import Foreign.C.Types
import WGPU.Raw.Types
import Prelude (pure, ($!))
import WGPU.Raw.Generated.Struct.WGPUChainedStruct

data WGPUMultisampleState = WGPUMultisampleState {
  nextInChain :: Ptr (WGPUChainedStruct),
  count :: Word32,
  mask :: Word32,
  alphaToCoverageEnabled :: CBool
}

instance Storable WGPUMultisampleState where
  sizeOf _ = (#size WGPUMultisampleState)
  alignment = sizeOf
  peek ptr = do
    nextInChain <- (#peek WGPUMultisampleState, nextInChain) ptr
    count <- (#peek WGPUMultisampleState, count) ptr
    mask <- (#peek WGPUMultisampleState, mask) ptr
    alphaToCoverageEnabled <- (#peek WGPUMultisampleState, alphaToCoverageEnabled) ptr
    pure $! WGPUMultisampleState{..}
  poke ptr WGPUMultisampleState{..} = do
    (#poke WGPUMultisampleState, nextInChain) ptr nextInChain
    (#poke WGPUMultisampleState, count) ptr count
    (#poke WGPUMultisampleState, mask) ptr mask
    (#poke WGPUMultisampleState, alphaToCoverageEnabled) ptr alphaToCoverageEnabled