{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- This file was generated by wgpu-raw-hs-codegen on:
--   2021-08-25T10:02:03.522705
-- Using wgpu-native git hash:
--   b10496e7eed9349f0fd541e6dfe5029cb436de74 wgpu-native (v0.9.2.2)

module WGPU.Raw.Generated.Struct.WGPURenderPassDepthStencilAttachment where

#include "wgpu.h"

import Data.Word (Word16, Word32, Word64)
import Data.Int (Int32)
import Foreign
import Foreign.C.Types
import WGPU.Raw.Types
import Prelude (pure, ($!))
import WGPU.Raw.Generated.Enum.WGPULoadOp
import WGPU.Raw.Generated.Enum.WGPUStoreOp
import WGPU.Raw.Generated.Enum.WGPULoadOp
import WGPU.Raw.Generated.Enum.WGPUStoreOp

data WGPURenderPassDepthStencilAttachment = WGPURenderPassDepthStencilAttachment {
  view :: WGPUTextureView,
  depthLoadOp :: WGPULoadOp,
  depthStoreOp :: WGPUStoreOp,
  clearDepth :: CFloat,
  depthReadOnly :: CBool,
  stencilLoadOp :: WGPULoadOp,
  stencilStoreOp :: WGPUStoreOp,
  clearStencil :: Word32,
  stencilReadOnly :: CBool
}

instance Storable WGPURenderPassDepthStencilAttachment where
  sizeOf _ = (#size WGPURenderPassDepthStencilAttachment)
  {-# INLINABLE sizeOf #-}
  alignment = sizeOf
  {-# INLINABLE alignment #-}
  peek ptr = do
    view <- (#peek WGPURenderPassDepthStencilAttachment, view) ptr
    depthLoadOp <- (#peek WGPURenderPassDepthStencilAttachment, depthLoadOp) ptr
    depthStoreOp <- (#peek WGPURenderPassDepthStencilAttachment, depthStoreOp) ptr
    clearDepth <- (#peek WGPURenderPassDepthStencilAttachment, clearDepth) ptr
    depthReadOnly <- (#peek WGPURenderPassDepthStencilAttachment, depthReadOnly) ptr
    stencilLoadOp <- (#peek WGPURenderPassDepthStencilAttachment, stencilLoadOp) ptr
    stencilStoreOp <- (#peek WGPURenderPassDepthStencilAttachment, stencilStoreOp) ptr
    clearStencil <- (#peek WGPURenderPassDepthStencilAttachment, clearStencil) ptr
    stencilReadOnly <- (#peek WGPURenderPassDepthStencilAttachment, stencilReadOnly) ptr
    pure $! WGPURenderPassDepthStencilAttachment{..}
  {-# INLINABLE peek #-}
  poke ptr WGPURenderPassDepthStencilAttachment{..} = do
    (#poke WGPURenderPassDepthStencilAttachment, view) ptr view
    (#poke WGPURenderPassDepthStencilAttachment, depthLoadOp) ptr depthLoadOp
    (#poke WGPURenderPassDepthStencilAttachment, depthStoreOp) ptr depthStoreOp
    (#poke WGPURenderPassDepthStencilAttachment, clearDepth) ptr clearDepth
    (#poke WGPURenderPassDepthStencilAttachment, depthReadOnly) ptr depthReadOnly
    (#poke WGPURenderPassDepthStencilAttachment, stencilLoadOp) ptr stencilLoadOp
    (#poke WGPURenderPassDepthStencilAttachment, stencilStoreOp) ptr stencilStoreOp
    (#poke WGPURenderPassDepthStencilAttachment, clearStencil) ptr clearStencil
    (#poke WGPURenderPassDepthStencilAttachment, stencilReadOnly) ptr stencilReadOnly
  {-# INLINABLE poke #-}