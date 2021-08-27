{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

-- |
-- Module      : WGPU.Internal.Device.
-- Description : Device (open connection to a device).
module WGPU.Internal.Device
  ( -- * Types
    Device (..),
    DeviceDescriptor (..),
    Features (..),
    Limits (..),

    -- * Functions
    requestDevice,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Default (Default, def)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word32)
import Foreign (Ptr, nullPtr)
import WGPU.Internal.Adapter (Adapter, adapterInst, wgpuAdapter)
import WGPU.Internal.ChainedStruct (ChainedStruct (EmptyChain, PtrChain))
import WGPU.Internal.Instance (Instance, wgpuHsInstance)
import WGPU.Internal.Memory
  ( ToRaw,
    evalContT,
    freeHaskellFunPtr,
    newEmptyMVar,
    putMVar,
    raw,
    rawPtr,
    showWithPtr,
    takeMVar,
    withCZeroingAfter,
  )
import WGPU.Raw.Generated.Enum.WGPUNativeFeature (WGPUNativeFeature)
import qualified WGPU.Raw.Generated.Enum.WGPUNativeFeature as WGPUNativeFeature
import qualified WGPU.Raw.Generated.Enum.WGPUNativeSType as WGPUSType
import qualified WGPU.Raw.Generated.Fun as RawFun
import qualified WGPU.Raw.Generated.Struct.WGPUDeviceDescriptor as WGPUDeviceDescriptor
import WGPU.Raw.Generated.Struct.WGPUDeviceExtras (WGPUDeviceExtras)
import qualified WGPU.Raw.Generated.Struct.WGPUDeviceExtras as WGPUDeviceExtras
import WGPU.Raw.Types (WGPUDevice (WGPUDevice), WGPURequestDeviceCallback)

-------------------------------------------------------------------------------

-- | An open connection to a graphics and/or compute device.
--
-- A 'Device' may be created using the 'requestDevice' function.
data Device = Device
  { deviceInst :: !Instance,
    wgpuDevice :: !WGPUDevice
  }

instance Show Device where
  show d =
    let Device _ (WGPUDevice ptr) = d
     in showWithPtr "Device" ptr

instance Eq Device where
  (==) d1 d2 =
    let Device _ (WGPUDevice d1_ptr) = d1
        Device _ (WGPUDevice d2_ptr) = d2
     in d1_ptr == d2_ptr

instance ToRaw Device WGPUDevice where
  raw = pure . wgpuDevice

-------------------------------------------------------------------------------

-- | Device features that are not guaranteed to be supported.
--
--   * NOTE: The Rust API currently has far more extensive @Features@. Perhaps
--     they have not yet been ported to the C API?
--     <https://docs.rs/wgpu-types/0.9.0/wgpu_types/struct.Features.html>
newtype Features = Features
  { textureAdapterSpecificFormatFeatures :: Bool
  }
  deriving (Eq, Show)

instance Default Features where
  def =
    Features
      { textureAdapterSpecificFormatFeatures = False
      }

instance ToRaw Features WGPUNativeFeature where
  raw Features {..} =
    pure $
      if textureAdapterSpecificFormatFeatures
        then WGPUNativeFeature.TEXTURE_ADAPTER_SPECIFIC_FORMAT_FEATURES
        else 0

-------------------------------------------------------------------------------

-- | Device limits.
--
-- Represents the set of limits an adapter/device supports.
data Limits = Limits
  { -- | Maximum allowed value for the width of a 1D texture.
    maxTextureDimension1D :: !Word32,
    -- | Maximum allowed value for the width and height of a 2D texture.
    maxTextureDimension2D :: !Word32,
    -- | Maximum allowed value for the width, height or depth of a 3D texture.
    maxTextureDimension3D :: !Word32,
    -- | Maximum allowed value for the array layers of a texture.
    maxTextureArrayLayers :: !Word32,
    -- | Amount of bind groups that can be attached to a pipeline at the same
    --   time.
    maxBindGroups :: !Word32,
    -- | Amount of storage buffer bindings that can be dynamic in a single
    --   pipeline.
    maxDynamicStorageBuffersPerPipelineLayout :: !Word32,
    -- | Amount of sampled textures visible in a single shader stage.
    maxStorageBuffersPerShaderStage :: !Word32,
    -- | Maximum size in bytes of a binding to a uniform buffer.
    maxStorageBufferBindingSize :: !Word32
  }
  deriving (Eq, Show)

instance Default Limits where
  def =
    Limits
      { maxTextureDimension1D = 0,
        maxTextureDimension2D = 0,
        maxTextureDimension3D = 0,
        maxTextureArrayLayers = 0,
        maxBindGroups = 0,
        maxDynamicStorageBuffersPerPipelineLayout = 0,
        maxStorageBuffersPerShaderStage = 0,
        maxStorageBufferBindingSize = 0
      }

-------------------------------------------------------------------------------

-- | Describes a 'Device'.
data DeviceDescriptor = DeviceDescriptor
  { -- | Debug label for the device.
    deviceLabel :: !Text,
    -- | Features that the device should support.
    features :: !Features,
    -- | Limits that the device should support (minimum values).
    limits :: !Limits
  }
  deriving (Eq, Show)

instance Default DeviceDescriptor where
  def =
    DeviceDescriptor
      { deviceLabel = Text.empty,
        features = def,
        limits = def
      }

instance ToRaw DeviceDescriptor WGPUDeviceExtras where
  raw DeviceDescriptor {..} = do
    chain_ptr <- raw (EmptyChain WGPUSType.DeviceExtras)
    label_ptr <- rawPtr deviceLabel
    n_nativeFeatures <- raw features
    pure
      WGPUDeviceExtras.WGPUDeviceExtras
        { chain = chain_ptr,
          maxTextureDimension1D = maxTextureDimension1D limits,
          maxTextureDimension2D = maxTextureDimension2D limits,
          maxTextureDimension3D = maxTextureDimension3D limits,
          maxTextureArrayLayers = maxTextureArrayLayers limits,
          maxBindGroups = maxBindGroups limits,
          maxDynamicStorageBuffersPerPipelineLayout =
            maxDynamicStorageBuffersPerPipelineLayout limits,
          maxStorageBuffersPerShaderStage =
            maxStorageBuffersPerShaderStage limits,
          maxStorageBufferBindingSize =
            maxStorageBufferBindingSize limits,
          nativeFeatures = n_nativeFeatures,
          label = label_ptr,
          tracePath = nullPtr
        }

-- | Requests a connection to a physical device, creating a logical device.
--
-- This action blocks until an available device is returned.
requestDevice ::
  MonadIO m =>
  -- | @Adapter@ for which the device will be returned.
  Adapter ->
  -- | The features and limits requested for the device.
  DeviceDescriptor ->
  -- | The returned @Device@, if it could be retrieved.
  m (Maybe Device)
requestDevice adapter deviceDescriptor = liftIO . evalContT $ do
  let inst = adapterInst adapter

  deviceMVar <- newEmptyMVar
  callback <- mkDeviceCallback (\d _ -> putMVar deviceMVar d)

  deviceExtras_ptr <- rawPtr deviceDescriptor
  nextInChain_ptr <- rawPtr (PtrChain WGPUSType.DeviceExtras deviceExtras_ptr)
  deviceDescriptor_ptr <-
    withCZeroingAfter $
      WGPUDeviceDescriptor.WGPUDeviceDescriptor
        { nextInChain = nextInChain_ptr
        }

  RawFun.wgpuAdapterRequestDevice
    (wgpuHsInstance inst)
    (wgpuAdapter adapter)
    deviceDescriptor_ptr
    callback
    nullPtr

  device <- takeMVar deviceMVar
  freeHaskellFunPtr callback

  pure $ case device of
    WGPUDevice ptr | ptr == nullPtr -> Nothing
    WGPUDevice _ -> Just (Device inst device)

mkDeviceCallback ::
  (MonadIO m) =>
  (WGPUDevice -> Ptr () -> IO ()) ->
  m WGPURequestDeviceCallback
mkDeviceCallback = liftIO . mkDeviceCallbackIO

foreign import ccall "wrapper"
  mkDeviceCallbackIO ::
    (WGPUDevice -> Ptr () -> IO ()) -> IO WGPURequestDeviceCallback
