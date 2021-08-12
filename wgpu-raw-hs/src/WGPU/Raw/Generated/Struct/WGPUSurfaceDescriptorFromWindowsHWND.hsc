{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- This file was generated by wgpu-raw-hs-codegen on:
--   2021-08-14T08:10:31.492798
-- Using wgpu-native git hash:
--   b10496e7eed9349f0fd541e6dfe5029cb436de74 wgpu-native (v0.9.2.2)

module WGPU.Raw.Generated.Struct.WGPUSurfaceDescriptorFromWindowsHWND where

#include "wgpu.h"

import Data.Word (Word16, Word32, Word64)
import Data.Int (Int32)
import Foreign
import Foreign.C.Types
import WGPU.Raw.Types
import Prelude (pure, ($!))
import WGPU.Raw.Generated.Struct.WGPUChainedStruct

data WGPUSurfaceDescriptorFromWindowsHWND = WGPUSurfaceDescriptorFromWindowsHWND {
  chain :: WGPUChainedStruct,
  hinstance :: Ptr (()),
  hwnd :: Ptr (())
}

instance Storable WGPUSurfaceDescriptorFromWindowsHWND where
  sizeOf _ = (#size WGPUSurfaceDescriptorFromWindowsHWND)
  alignment = sizeOf
  peek ptr = do
    chain <- (#peek WGPUSurfaceDescriptorFromWindowsHWND, chain) ptr
    hinstance <- (#peek WGPUSurfaceDescriptorFromWindowsHWND, hinstance) ptr
    hwnd <- (#peek WGPUSurfaceDescriptorFromWindowsHWND, hwnd) ptr
    pure $! WGPUSurfaceDescriptorFromWindowsHWND{..}
  poke ptr WGPUSurfaceDescriptorFromWindowsHWND{..} = do
    (#poke WGPUSurfaceDescriptorFromWindowsHWND, chain) ptr chain
    (#poke WGPUSurfaceDescriptorFromWindowsHWND, hinstance) ptr hinstance
    (#poke WGPUSurfaceDescriptorFromWindowsHWND, hwnd) ptr hwnd