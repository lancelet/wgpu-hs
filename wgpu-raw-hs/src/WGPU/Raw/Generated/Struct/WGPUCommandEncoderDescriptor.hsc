{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- This file was generated by wgpu-raw-hs-codegen on:
--   2021-08-14T11:57:14.367365
-- Using wgpu-native git hash:
--   b10496e7eed9349f0fd541e6dfe5029cb436de74 wgpu-native (v0.9.2.2)

module WGPU.Raw.Generated.Struct.WGPUCommandEncoderDescriptor where

#include "wgpu.h"

import Data.Word (Word16, Word32, Word64)
import Data.Int (Int32)
import Foreign
import Foreign.C.Types
import WGPU.Raw.Types
import Prelude (pure, ($!))
import WGPU.Raw.Generated.Struct.WGPUChainedStruct

data WGPUCommandEncoderDescriptor = WGPUCommandEncoderDescriptor {
  nextInChain :: Ptr (WGPUChainedStruct),
  label :: Ptr (CChar)
}

instance Storable WGPUCommandEncoderDescriptor where
  sizeOf _ = (#size WGPUCommandEncoderDescriptor)
  alignment = sizeOf
  peek ptr = do
    nextInChain <- (#peek WGPUCommandEncoderDescriptor, nextInChain) ptr
    label <- (#peek WGPUCommandEncoderDescriptor, label) ptr
    pure $! WGPUCommandEncoderDescriptor{..}
  poke ptr WGPUCommandEncoderDescriptor{..} = do
    (#poke WGPUCommandEncoderDescriptor, nextInChain) ptr nextInChain
    (#poke WGPUCommandEncoderDescriptor, label) ptr label