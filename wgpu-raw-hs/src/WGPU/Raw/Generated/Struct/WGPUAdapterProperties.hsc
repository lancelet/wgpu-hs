{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- This file was generated by wgpu-raw-hs-codegen on:
--   2021-08-25T10:02:03.522705
-- Using wgpu-native git hash:
--   b10496e7eed9349f0fd541e6dfe5029cb436de74 wgpu-native (v0.9.2.2)

module WGPU.Raw.Generated.Struct.WGPUAdapterProperties where

#include "wgpu.h"

import Data.Word (Word16, Word32, Word64)
import Data.Int (Int32)
import Foreign
import Foreign.C.Types
import WGPU.Raw.Types
import Prelude (pure, ($!))
import WGPU.Raw.Generated.Enum.WGPUAdapterType
import WGPU.Raw.Generated.Enum.WGPUBackendType
import WGPU.Raw.Generated.Struct.WGPUChainedStruct

data WGPUAdapterProperties = WGPUAdapterProperties {
  nextInChain :: Ptr (WGPUChainedStruct),
  deviceID :: Word32,
  vendorID :: Word32,
  name :: Ptr (CChar),
  driverDescription :: Ptr (CChar),
  adapterType :: WGPUAdapterType,
  backendType :: WGPUBackendType
}

instance Storable WGPUAdapterProperties where
  sizeOf _ = (#size WGPUAdapterProperties)
  {-# INLINABLE sizeOf #-}
  alignment = sizeOf
  {-# INLINABLE alignment #-}
  peek ptr = do
    nextInChain <- (#peek WGPUAdapterProperties, nextInChain) ptr
    deviceID <- (#peek WGPUAdapterProperties, deviceID) ptr
    vendorID <- (#peek WGPUAdapterProperties, vendorID) ptr
    name <- (#peek WGPUAdapterProperties, name) ptr
    driverDescription <- (#peek WGPUAdapterProperties, driverDescription) ptr
    adapterType <- (#peek WGPUAdapterProperties, adapterType) ptr
    backendType <- (#peek WGPUAdapterProperties, backendType) ptr
    pure $! WGPUAdapterProperties{..}
  {-# INLINABLE peek #-}
  poke ptr WGPUAdapterProperties{..} = do
    (#poke WGPUAdapterProperties, nextInChain) ptr nextInChain
    (#poke WGPUAdapterProperties, deviceID) ptr deviceID
    (#poke WGPUAdapterProperties, vendorID) ptr vendorID
    (#poke WGPUAdapterProperties, name) ptr name
    (#poke WGPUAdapterProperties, driverDescription) ptr driverDescription
    (#poke WGPUAdapterProperties, adapterType) ptr adapterType
    (#poke WGPUAdapterProperties, backendType) ptr backendType
  {-# INLINABLE poke #-}