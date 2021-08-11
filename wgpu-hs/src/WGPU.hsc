{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WGPU
  ( -- * Types
    Surface,
    NativeFeature (..),
    DeviceExtras (..),

    -- * Functions

    -- ** Initialization
    createSurface,
    requestCompatibleAdapter,
  )
where

#include "wgpu.h"

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word32)
import Foreign
  (
    Ptr,
    Storable (alignment, sizeOf, peek, poke),
    nullPtr,
    peekByteOff,
    plusPtr,
  )
import Foreign.C.String (peekCString)
import qualified SDL (Window)
import Unsafe.Coerce (unsafeCoerce)

-------------------------------------------------------------------------------
-- Helper C functions from this package

foreign import ccall "wgpuhs_create_surface"
  wgpuhs_create_surface :: Ptr () -> IO (Ptr ())

foreign import ccall "wgpuhs_request_compatible_adapter"
  wgpuhs_request_compatible_adapter :: Ptr () -> IO (Ptr ())

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- Types from webgpu.h

newtype Adapter = Adapter (Ptr ())

newtype Surface = Surface (Ptr ())

-- Types from wgpu.h

data NativeFeature = TEXTURE_ADAPTER_SPECIFIC_FORMAT_FEATURES

instance Storable NativeFeature where
  sizeOf _ = (#size uint32_t)
  alignment = sizeOf
  peek ptr = undefined
  poke ptr n = undefined


data DeviceExtras = DeviceExtras
  { deviceExtrasMaxTextureDimension1D :: !Word32,
    deviceExtrasMaxTextureDimension2D :: !Word32,
    deviceExtrasMaxTextureDimension3D :: !Word32,
    deviceExtrasMaxTextureArrayLayers :: !Word32,
    deviceExtrasMaxBindGroups :: !Word32,
    deviceExtrasMaxDynamicStorageBuffersPerPipelineLayout :: !Word32,
    deviceExtrasMaxStorageBuffersPerShaderStage :: !Word32,
    deviceExtrasMaxStorageBufferBindingSize :: !Word32,
    deviceExtrasNativeFeatures :: NativeFeature,
    deviceExtrasLabel :: !Text,
    deviceExtrasTracePath :: !Text
  }

instance Storable DeviceExtras where
  sizeOf _ = (#size WGPUDeviceExtras)
  alignment = sizeOf
  peek ptr = do
    td1d <- (#peek WGPUDeviceExtras, maxTextureDimension1D) ptr
    td2d <- (#peek WGPUDeviceExtras, maxTextureDimension2D) ptr
    td3d <- (#peek WGPUDeviceExtras, maxTextureDimension3D) ptr
    tal <- (#peek WGPUDeviceExtras, maxTextureArrayLayers) ptr
    bg <- (#peek WGPUDeviceExtras, maxBindGroups) ptr
    dsb <-
      (#peek WGPUDeviceExtras, maxDynamicStorageBuffersPerPipelineLayout) ptr
    bpss <- (#peek WGPUDeviceExtras, maxStorageBuffersPerShaderStage) ptr
    bbs <- (#peek WGPUDeviceExtras, maxStorageBufferBindingSize) ptr
    enf <- (#peek WGPUDeviceExtras, nativeFeatures) ptr
    label <- peekCString $ #{ptr WGPUDeviceExtras, label} ptr
    tracePath <-
      peekCString $ #{ptr WGPUDeviceExtras, tracePath} ptr
    let labelText, tracePathText :: Text
        labelText = Text.pack label
        tracePathText = Text.pack tracePath
    pure $! DeviceExtras td1d td2d td3d tal bg dsb bpss bbs enf labelText tracePathText
  poke
    ptr
    (DeviceExtras td1d td2d td3d tal bg dsb bpss bbs enf label tracePath) =
      undefined

-------------------------------------------------------------------------------
-- Operations

-- | Create a WGPU surface for an SDL window.
createSurface ::
  -- | SDL window in which to create the WGPU surface.
  SDL.Window ->
  -- | WGPU surface for drawing.
  IO Surface
createSurface window = do
  -- coerce the SDL window back to a raw pointer
  let windowPtr :: Ptr ()
      windowPtr = unsafeCoerce window

  -- create the surface
  surfacePtr <- wgpuhs_create_surface windowPtr

  -- report an error if we failed
  if surfacePtr == nullPtr
    then error "WGPUHS: ERROR: Could not create WGPU surface"
    else pure (Surface surfacePtr)

requestCompatibleAdapter :: Surface -> IO Adapter
requestCompatibleAdapter (Surface surfacePtr) = do
  adapterPtr <- wgpuhs_request_compatible_adapter surfacePtr
  if adapterPtr == nullPtr
    then error "WGPUHS: ERROR: Could not obtain compatible Adapter"
    else pure (Adapter adapterPtr)
