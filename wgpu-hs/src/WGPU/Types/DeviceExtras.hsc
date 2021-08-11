{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-- |

module WGPU.Types.DeviceExtras where

#include "wgpu.h"

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (ContT (ContT), evalContT)
import Data.Kind (Type)
import Data.Text (Text)
import Foreign
import Foreign.C.String
import WGPU.Chain
import WGPU.Types.NativeFeature (NativeFeature(NativeFeature))
import qualified Data.Text as Text

data DeviceExtras (es :: [Type]) = DeviceExtras
  { chain                                     :: !(Chain es),
    maxTextureDimension1D                     :: !Word32,
    maxTextureDimension2D                     :: !Word32,
    maxTextureDimension3D                     :: !Word32,
    maxBindGroups                             :: !Word32,
    maxDynamicStorageBuffersPerPipelineLayout :: !Word32,
    maxStorageBuffersPerShaderStage           :: !Word32,
    maxStorageBufferBindingSize               :: !Word32,
    nativeFeatures                            :: !NativeFeature,
    label                                     :: !Text,
    tracePath                                 :: !Text
  }

instance HasChainPtr (DeviceExtras es) where
  peekChainPtr ptr = (#peek WGPUDeviceExtras, chain) ptr
  pokeChainPtr = undefined

instance (PeekChain es, PokeChain es) => Storable (DeviceExtras es) where

  sizeOf _ = (#size WGPUDeviceExtras)
  alignment = sizeOf

  peek ptr = do
    chain                                     <- peekChain $ (#ptr WGPUDeviceExtras, chain) ptr
    maxTextureDimension1D                     <- (#peek WGPUDeviceExtras, maxTextureDimension1D) ptr
    maxTextureDimension2D                     <- (#peek WGPUDeviceExtras, maxTextureDimension2D) ptr
    maxTextureDimension3D                     <- (#peek WGPUDeviceExtras, maxTextureDimension3D) ptr
    maxBindGroups                             <- (#peek WGPUDeviceExtras, maxBindGroups) ptr
    maxDynamicStorageBuffersPerPipelineLayout <- (#peek WGPUDeviceExtras, maxDynamicStorageBuffersPerPipelineLayout) ptr
    maxStorageBuffersPerShaderStage           <- (#peek WGPUDeviceExtras, maxStorageBuffersPerShaderStage) ptr
    maxStorageBufferBindingSize               <- (#peek WGPUDeviceExtras, maxStorageBufferBindingSize) ptr
    c_nativeFeatures                          <- (#peek WGPUDeviceExtras, nativeFeatures) ptr
    c_label                                   <- peekCString $ (#ptr WGPUDeviceExtras, label) ptr
    c_tracePath                               <- peekCString $ (#ptr WGPUDeviceExtras, tracePath) ptr
    let
      nativeFeatures = NativeFeature c_nativeFeatures
      label          = Text.pack c_label
      tracePath      = Text.pack c_tracePath
    pure $! DeviceExtras{..}

  poke ptr DeviceExtras{..} =
    evalContT $ do
      let
        NativeFeature c_nativeFeatures = nativeFeatures
      -- TODO: this will break - cstrings don't live long enough
      c_label      <- ContT $ withCString (Text.unpack label)
      c_tracePath  <- ContT $ withCString (Text.unpack tracePath)
      (_, c_chain) <- ContT $ withChain chain
      lift $ (#poke WGPUDeviceExtras, chain) ptr c_chain
      lift $ (#poke WGPUDeviceExtras, maxTextureDimension1D) ptr maxTextureDimension1D
      lift $ (#poke WGPUDeviceExtras, maxTextureDimension2D) ptr maxTextureDimension2D
      lift $ (#poke WGPUDeviceExtras, maxTextureDimension3D) ptr maxTextureDimension3D
      lift $ (#poke WGPUDeviceExtras, maxBindGroups) ptr maxBindGroups
      lift $ (#poke WGPUDeviceExtras, maxDynamicStorageBuffersPerPipelineLayout) ptr maxDynamicStorageBuffersPerPipelineLayout
      lift $ (#poke WGPUDeviceExtras, maxStorageBuffersPerShaderStage) ptr maxStorageBuffersPerShaderStage
      lift $ (#poke WGPUDeviceExtras, maxStorageBufferBindingSize) ptr maxStorageBufferBindingSize
      lift $ (#poke WGPUDeviceExtras, nativeFeatures) ptr c_nativeFeatures
      lift $ (#poke WGPUDeviceExtras, label) ptr c_label
      lift $ (#poke WGPUDeviceExtras, tracePath) ptr c_tracePath
