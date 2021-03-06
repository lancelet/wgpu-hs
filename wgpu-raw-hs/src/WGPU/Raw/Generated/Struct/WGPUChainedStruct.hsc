{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- This file was generated by wgpu-raw-hs-codegen on:
--   2021-08-25T10:02:03.522705
-- Using wgpu-native git hash:
--   b10496e7eed9349f0fd541e6dfe5029cb436de74 wgpu-native (v0.9.2.2)

module WGPU.Raw.Generated.Struct.WGPUChainedStruct where

#include "wgpu.h"

import Data.Word (Word16, Word32, Word64)
import Data.Int (Int32)
import Foreign
import Foreign.C.Types
import WGPU.Raw.Types
import Prelude (pure, ($!))
import WGPU.Raw.Generated.Enum.WGPUSType

data WGPUChainedStruct = WGPUChainedStruct {
  next :: Ptr (WGPUChainedStruct),
  sType :: WGPUSType
}

instance Storable WGPUChainedStruct where
  sizeOf _ = (#size WGPUChainedStruct)
  {-# INLINABLE sizeOf #-}
  alignment = sizeOf
  {-# INLINABLE alignment #-}
  peek ptr = do
    next <- (#peek WGPUChainedStruct, next) ptr
    sType <- (#peek WGPUChainedStruct, sType) ptr
    pure $! WGPUChainedStruct{..}
  {-# INLINABLE peek #-}
  poke ptr WGPUChainedStruct{..} = do
    (#poke WGPUChainedStruct, next) ptr next
    (#poke WGPUChainedStruct, sType) ptr sType
  {-# INLINABLE poke #-}