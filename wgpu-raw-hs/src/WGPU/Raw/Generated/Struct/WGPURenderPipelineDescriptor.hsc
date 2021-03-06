{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- This file was generated by wgpu-raw-hs-codegen on:
--   2021-08-25T10:02:03.522705
-- Using wgpu-native git hash:
--   b10496e7eed9349f0fd541e6dfe5029cb436de74 wgpu-native (v0.9.2.2)

module WGPU.Raw.Generated.Struct.WGPURenderPipelineDescriptor where

#include "wgpu.h"

import Data.Word (Word16, Word32, Word64)
import Data.Int (Int32)
import Foreign
import Foreign.C.Types
import WGPU.Raw.Types
import Prelude (pure, ($!))
import WGPU.Raw.Generated.Struct.WGPUChainedStruct
import WGPU.Raw.Generated.Struct.WGPUVertexState
import WGPU.Raw.Generated.Struct.WGPUPrimitiveState
import WGPU.Raw.Generated.Struct.WGPUDepthStencilState
import WGPU.Raw.Generated.Struct.WGPUMultisampleState
import WGPU.Raw.Generated.Struct.WGPUFragmentState

data WGPURenderPipelineDescriptor = WGPURenderPipelineDescriptor {
  nextInChain :: Ptr (WGPUChainedStruct),
  label :: Ptr (CChar),
  layout :: WGPUPipelineLayout,
  vertex :: WGPUVertexState,
  primitive :: WGPUPrimitiveState,
  depthStencil :: Ptr (WGPUDepthStencilState),
  multisample :: WGPUMultisampleState,
  fragment :: Ptr (WGPUFragmentState)
}

instance Storable WGPURenderPipelineDescriptor where
  sizeOf _ = (#size WGPURenderPipelineDescriptor)
  {-# INLINABLE sizeOf #-}
  alignment = sizeOf
  {-# INLINABLE alignment #-}
  peek ptr = do
    nextInChain <- (#peek WGPURenderPipelineDescriptor, nextInChain) ptr
    label <- (#peek WGPURenderPipelineDescriptor, label) ptr
    layout <- (#peek WGPURenderPipelineDescriptor, layout) ptr
    vertex <- (#peek WGPURenderPipelineDescriptor, vertex) ptr
    primitive <- (#peek WGPURenderPipelineDescriptor, primitive) ptr
    depthStencil <- (#peek WGPURenderPipelineDescriptor, depthStencil) ptr
    multisample <- (#peek WGPURenderPipelineDescriptor, multisample) ptr
    fragment <- (#peek WGPURenderPipelineDescriptor, fragment) ptr
    pure $! WGPURenderPipelineDescriptor{..}
  {-# INLINABLE peek #-}
  poke ptr WGPURenderPipelineDescriptor{..} = do
    (#poke WGPURenderPipelineDescriptor, nextInChain) ptr nextInChain
    (#poke WGPURenderPipelineDescriptor, label) ptr label
    (#poke WGPURenderPipelineDescriptor, layout) ptr layout
    (#poke WGPURenderPipelineDescriptor, vertex) ptr vertex
    (#poke WGPURenderPipelineDescriptor, primitive) ptr primitive
    (#poke WGPURenderPipelineDescriptor, depthStencil) ptr depthStencil
    (#poke WGPURenderPipelineDescriptor, multisample) ptr multisample
    (#poke WGPURenderPipelineDescriptor, fragment) ptr fragment
  {-# INLINABLE poke #-}