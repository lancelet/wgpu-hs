{-# LANGUAGE RecordWildCards #-}
-- |

module WGPU.Types.DeviceExtras where

#include "wgpu.h"

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word32)
import Foreign
  (
    Ptr,
    Storable,
    alignment,
    peek,
    peekByteOff,
    plusPtr,
    poke,
    sizeOf,
  )
import Foreign.C.String (peekCString)
import WGPU.Types.Chain (Chain, CanChain)
import WGPU.Types.NativeFeature (NativeFeature(NativeFeature))

data DeviceExtras = DeviceExtras (es :: [Type])
  { chain                                     :: !Chain es,
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
  deriving (Eq, Show)

instance Chainable DeviceExtras where
  attachChain x s = s { chain = newChainPtr x }

instance Storable DeviceExtras where

  sizeOf _ = (#size WGPUDeviceExtras)
  alignment = sizeOf

  peek ptr = do
    chain                                     <- pure nullChainPtr
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

  poke ptr DeviceExtras{..} = do
    let
      NativeFeature c_nativeFeatures = nativeFeatures
    withChainItem chain $ c_chain ->
    withCString (Text.unpack label) $ c_label -> do
      withCString (Text.unpack tracePath) $ c_
