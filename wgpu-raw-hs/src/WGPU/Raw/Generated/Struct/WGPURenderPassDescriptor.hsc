{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- This file was generated by wgpu-raw-hs-codegen on:
--   2021-08-15T08:56:06.968390
-- Using wgpu-native git hash:
--   b10496e7eed9349f0fd541e6dfe5029cb436de74 wgpu-native (v0.9.2.2)

module WGPU.Raw.Generated.Struct.WGPURenderPassDescriptor where

#include "wgpu.h"

import Data.Word (Word16, Word32, Word64)
import Data.Int (Int32)
import Foreign
import Foreign.C.Types
import WGPU.Raw.Types
import Prelude (pure, ($!))
import WGPU.Raw.Generated.Struct.WGPUChainedStruct
import WGPU.Raw.Generated.Struct.WGPURenderPassColorAttachment
import WGPU.Raw.Generated.Struct.WGPURenderPassDepthStencilAttachment

data WGPURenderPassDescriptor = WGPURenderPassDescriptor {
  nextInChain :: Ptr (WGPUChainedStruct),
  label :: Ptr (CChar),
  colorAttachmentCount :: Word32,
  colorAttachments :: Ptr (WGPURenderPassColorAttachment),
  depthStencilAttachment :: Ptr (WGPURenderPassDepthStencilAttachment),
  occlusionQuerySet :: WGPUQuerySet
}

instance Storable WGPURenderPassDescriptor where
  sizeOf _ = (#size WGPURenderPassDescriptor)
  alignment = sizeOf
  peek ptr = do
    nextInChain <- (#peek WGPURenderPassDescriptor, nextInChain) ptr
    label <- (#peek WGPURenderPassDescriptor, label) ptr
    colorAttachmentCount <- (#peek WGPURenderPassDescriptor, colorAttachmentCount) ptr
    colorAttachments <- (#peek WGPURenderPassDescriptor, colorAttachments) ptr
    depthStencilAttachment <- (#peek WGPURenderPassDescriptor, depthStencilAttachment) ptr
    occlusionQuerySet <- (#peek WGPURenderPassDescriptor, occlusionQuerySet) ptr
    pure $! WGPURenderPassDescriptor{..}
  poke ptr WGPURenderPassDescriptor{..} = do
    (#poke WGPURenderPassDescriptor, nextInChain) ptr nextInChain
    (#poke WGPURenderPassDescriptor, label) ptr label
    (#poke WGPURenderPassDescriptor, colorAttachmentCount) ptr colorAttachmentCount
    (#poke WGPURenderPassDescriptor, colorAttachments) ptr colorAttachments
    (#poke WGPURenderPassDescriptor, depthStencilAttachment) ptr depthStencilAttachment
    (#poke WGPURenderPassDescriptor, occlusionQuerySet) ptr occlusionQuerySet