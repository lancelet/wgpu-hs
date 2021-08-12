{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- This file was generated by wgpu-raw-hs-codegen on:
--   2021-08-14T08:10:31.492798
-- Using wgpu-native git hash:
--   b10496e7eed9349f0fd541e6dfe5029cb436de74 wgpu-native (v0.9.2.2)

module WGPU.Raw.Generated.Struct.WGPUColor where

#include "wgpu.h"

import Data.Word (Word16, Word32, Word64)
import Data.Int (Int32)
import Foreign
import Foreign.C.Types
import WGPU.Raw.Types
import Prelude (pure, ($!))


data WGPUColor = WGPUColor {
  r :: CDouble,
  g :: CDouble,
  b :: CDouble,
  a :: CDouble
}

instance Storable WGPUColor where
  sizeOf _ = (#size WGPUColor)
  alignment = sizeOf
  peek ptr = do
    r <- (#peek WGPUColor, r) ptr
    g <- (#peek WGPUColor, g) ptr
    b <- (#peek WGPUColor, b) ptr
    a <- (#peek WGPUColor, a) ptr
    pure $! WGPUColor{..}
  poke ptr WGPUColor{..} = do
    (#poke WGPUColor, r) ptr r
    (#poke WGPUColor, g) ptr g
    (#poke WGPUColor, b) ptr b
    (#poke WGPUColor, a) ptr a