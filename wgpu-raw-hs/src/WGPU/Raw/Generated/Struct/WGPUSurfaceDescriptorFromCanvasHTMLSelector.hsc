{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- This file was generated by wgpu-raw-hs-codegen on:
--   2021-08-15T08:56:06.968390
-- Using wgpu-native git hash:
--   b10496e7eed9349f0fd541e6dfe5029cb436de74 wgpu-native (v0.9.2.2)

module WGPU.Raw.Generated.Struct.WGPUSurfaceDescriptorFromCanvasHTMLSelector where

#include "wgpu.h"

import Data.Word (Word16, Word32, Word64)
import Data.Int (Int32)
import Foreign
import Foreign.C.Types
import WGPU.Raw.Types
import Prelude (pure, ($!))
import WGPU.Raw.Generated.Struct.WGPUChainedStruct

data WGPUSurfaceDescriptorFromCanvasHTMLSelector = WGPUSurfaceDescriptorFromCanvasHTMLSelector {
  chain :: WGPUChainedStruct,
  selector :: Ptr (CChar)
}

instance Storable WGPUSurfaceDescriptorFromCanvasHTMLSelector where
  sizeOf _ = (#size WGPUSurfaceDescriptorFromCanvasHTMLSelector)
  alignment = sizeOf
  peek ptr = do
    chain <- (#peek WGPUSurfaceDescriptorFromCanvasHTMLSelector, chain) ptr
    selector <- (#peek WGPUSurfaceDescriptorFromCanvasHTMLSelector, selector) ptr
    pure $! WGPUSurfaceDescriptorFromCanvasHTMLSelector{..}
  poke ptr WGPUSurfaceDescriptorFromCanvasHTMLSelector{..} = do
    (#poke WGPUSurfaceDescriptorFromCanvasHTMLSelector, chain) ptr chain
    (#poke WGPUSurfaceDescriptorFromCanvasHTMLSelector, selector) ptr selector