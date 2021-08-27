{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : WGPU.Internal.Adapter
-- Description : Adapter (physical device).
module WGPU.Internal.Adapter
  ( -- * Types
    Adapter (..),
    AdapterType (..),
    BackendType (..),
    AdapterProperties (..),

    -- * Functions
    requestAdapter,
    getAdapterProperties,
    adapterPropertiesToText,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word32)
import Foreign (nullPtr)
import Foreign.Ptr (Ptr)
import Text.Printf (printf)
import WGPU.Internal.ChainedStruct (ChainedStruct (EmptyChain))
import WGPU.Internal.Instance (Instance, wgpuHsInstance)
import WGPU.Internal.Memory
  ( FromRaw,
    ToRaw,
    allocaC,
    evalContT,
    freeHaskellFunPtr,
    fromRaw,
    fromRawPtr,
    newEmptyMVar,
    putMVar,
    raw,
    rawPtr,
    showWithPtr,
    takeMVar,
  )
import WGPU.Internal.Surface (Surface, surfaceInst)
import WGPU.Raw.Generated.Enum.WGPUAdapterType (WGPUAdapterType)
import qualified WGPU.Raw.Generated.Enum.WGPUAdapterType as WGPUAdapterType
import WGPU.Raw.Generated.Enum.WGPUBackendType (WGPUBackendType)
import qualified WGPU.Raw.Generated.Enum.WGPUBackendType as WGPUBackendType
import qualified WGPU.Raw.Generated.Enum.WGPUNativeSType as WGPUSType
import qualified WGPU.Raw.Generated.Fun as RawFun
import WGPU.Raw.Generated.Struct.WGPUAdapterProperties (WGPUAdapterProperties)
import qualified WGPU.Raw.Generated.Struct.WGPUAdapterProperties as WGPUAdapterProperties
import WGPU.Raw.Generated.Struct.WGPURequestAdapterOptions
  ( WGPURequestAdapterOptions,
  )
import qualified WGPU.Raw.Generated.Struct.WGPURequestAdapterOptions as WGPURequestAdapterOptions
import WGPU.Raw.Types
  ( WGPUAdapter (WGPUAdapter),
    WGPUInstance (WGPUInstance),
    WGPURequestAdapterCallback,
  )

-------------------------------------------------------------------------------

-- | Handle to a physical graphics and/or compute device.
--
-- Request an 'Adapter' for a 'Surface' using the 'requestAdapter' function.
data Adapter = Adapter
  { adapterInst :: !Instance,
    wgpuAdapter :: !WGPUAdapter
  }

instance Show Adapter where
  show a =
    let Adapter _ (WGPUAdapter ptr) = a
     in showWithPtr "Adapter" ptr

instance Eq Adapter where
  (==) a1 a2 =
    let Adapter _ (WGPUAdapter a1_ptr) = a1
        Adapter _ (WGPUAdapter a2_ptr) = a2
     in a1_ptr == a2_ptr

instance ToRaw Adapter WGPUAdapter where
  raw = pure . wgpuAdapter

-------------------------------------------------------------------------------

-- | Physical device type.
data AdapterType
  = AdapterTypeDiscreteGPU
  | AdapterTypeIntegratedGPU
  | AdapterTypeCPU
  | AdapterTypeUnknown
  deriving (Eq, Show)

instance ToRaw AdapterType WGPUAdapterType where
  raw adapterType = case adapterType of
    AdapterTypeDiscreteGPU -> pure WGPUAdapterType.DiscreteGPU
    AdapterTypeIntegratedGPU -> pure WGPUAdapterType.IntegratedGPU
    AdapterTypeCPU -> pure WGPUAdapterType.CPU
    AdapterTypeUnknown -> pure WGPUAdapterType.Unknown

instance FromRaw WGPUAdapterType AdapterType where
  fromRaw wAdapterType = pure $ case wAdapterType of
    WGPUAdapterType.DiscreteGPU -> AdapterTypeDiscreteGPU
    WGPUAdapterType.IntegratedGPU -> AdapterTypeIntegratedGPU
    WGPUAdapterType.CPU -> AdapterTypeCPU
    WGPUAdapterType.Unknown -> AdapterTypeUnknown
    _ -> AdapterTypeUnknown

-------------------------------------------------------------------------------

-- | Backends supported by WGPU.
data BackendType
  = BackendTypeNull
  | BackendTypeD3D11
  | BackendTypeD3D12
  | BackendTypeMetal
  | BackendTypeVulkan
  | BackendTypeOpenGL
  | BackendTypeOpenGLES
  deriving (Eq, Show)

instance ToRaw BackendType WGPUBackendType where
  raw backendType = case backendType of
    BackendTypeNull -> pure WGPUBackendType.Null
    BackendTypeD3D11 -> pure WGPUBackendType.D3D11
    BackendTypeD3D12 -> pure WGPUBackendType.D3D12
    BackendTypeMetal -> pure WGPUBackendType.Metal
    BackendTypeVulkan -> pure WGPUBackendType.Vulkan
    BackendTypeOpenGL -> pure WGPUBackendType.OpenGL
    BackendTypeOpenGLES -> pure WGPUBackendType.OpenGLES

instance FromRaw WGPUBackendType BackendType where
  fromRaw wBackendType = pure $ case wBackendType of
    WGPUBackendType.Null -> BackendTypeNull
    WGPUBackendType.D3D11 -> BackendTypeD3D11
    WGPUBackendType.D3D12 -> BackendTypeD3D12
    WGPUBackendType.Metal -> BackendTypeMetal
    WGPUBackendType.Vulkan -> BackendTypeVulkan
    WGPUBackendType.OpenGL -> BackendTypeOpenGL
    WGPUBackendType.OpenGLES -> BackendTypeOpenGLES
    _ -> BackendTypeNull

-------------------------------------------------------------------------------

data AdapterProperties = AdapterProperties
  { deviceID :: !Word32,
    vendorID :: !Word32,
    adapterName :: !Text,
    driverDescription :: !Text,
    adapterType :: !AdapterType,
    backendType :: !BackendType
  }
  deriving (Eq, Show)

instance ToRaw AdapterProperties WGPUAdapterProperties where
  raw AdapterProperties {..} = do
    chain_ptr <- rawPtr (EmptyChain WGPUSType.AdapterExtras)
    name_ptr <- rawPtr adapterName
    driverDescription_ptr <- rawPtr driverDescription
    n_adapterType <- raw adapterType
    n_backendType <- raw backendType
    pure
      WGPUAdapterProperties.WGPUAdapterProperties
        { nextInChain = chain_ptr,
          deviceID = deviceID,
          vendorID = vendorID,
          name = name_ptr,
          driverDescription = driverDescription_ptr,
          adapterType = n_adapterType,
          backendType = n_backendType
        }

instance FromRaw WGPUAdapterProperties AdapterProperties where
  fromRaw WGPUAdapterProperties.WGPUAdapterProperties {..} = do
    n_adapterName <- fromRaw name
    n_driverDescription <- fromRaw driverDescription
    n_adapterType <- fromRaw adapterType
    n_backendType <- fromRaw backendType
    pure
      AdapterProperties
        { deviceID = deviceID,
          vendorID = vendorID,
          adapterName = n_adapterName,
          driverDescription = n_driverDescription,
          adapterType = n_adapterType,
          backendType = n_backendType
        }

-------------------------------------------------------------------------------

-- | Request an 'Adapter' that is compatible with a given 'Surface'.
--
-- This action blocks until an available adapter is returned.
requestAdapter ::
  (MonadIO m) =>
  -- | Existing surface for which to request an @Adapter@.
  Surface ->
  -- | The returned @Adapter@, if it could be retrieved.
  m (Maybe Adapter)
requestAdapter surface = liftIO . evalContT $ do
  let inst = surfaceInst surface

  adaptmv <- newEmptyMVar
  callback <- mkAdapterCallback (\a _ -> putMVar adaptmv a)

  requestAdapterOptions_ptr <- rawPtr (RequestAdapterOptions surface)
  RawFun.wgpuInstanceRequestAdapter
    (wgpuHsInstance inst)
    (WGPUInstance nullPtr)
    requestAdapterOptions_ptr
    callback
    nullPtr

  adapter <- takeMVar adaptmv
  freeHaskellFunPtr callback

  pure $ case adapter of
    WGPUAdapter ptr | ptr == nullPtr -> Nothing
    WGPUAdapter _ -> Just (Adapter inst adapter)

mkAdapterCallback ::
  MonadIO m =>
  (WGPUAdapter -> Ptr () -> IO ()) ->
  m WGPURequestAdapterCallback
mkAdapterCallback = liftIO . mkAdapterCallbackIO

foreign import ccall "wrapper"
  mkAdapterCallbackIO ::
    (WGPUAdapter -> Ptr () -> IO ()) -> IO WGPURequestAdapterCallback

newtype RequestAdapterOptions = RequestAdapterOptions {compatibleSurface :: Surface}

instance ToRaw RequestAdapterOptions WGPURequestAdapterOptions where
  raw RequestAdapterOptions {..} = do
    n_surface <- raw compatibleSurface
    pure
      WGPURequestAdapterOptions.WGPURequestAdapterOptions
        { nextInChain = nullPtr,
          compatibleSurface = n_surface
        }

-------------------------------------------------------------------------------

-- | Get information about an adapter.
getAdapterProperties :: MonadIO m => Adapter -> m AdapterProperties
getAdapterProperties adapter = liftIO $
  evalContT $ do
    wgpuAdapterProperties_ptr <- allocaC
    RawFun.wgpuAdapterGetProperties
      (wgpuHsInstance . adapterInst $ adapter)
      (wgpuAdapter adapter)
      wgpuAdapterProperties_ptr
    fromRawPtr wgpuAdapterProperties_ptr

-- | Format adapter properties into a multi-line block of text.
--
-- This can be useful for debugging purposes.
adapterPropertiesToText :: AdapterProperties -> Text
adapterPropertiesToText AdapterProperties {..} =
  Text.unlines
    [ "Adapter Properties:",
      "  device ID   : " <> Text.pack (printf "0x%08x" deviceID),
      "  vendor ID   : " <> Text.pack (printf "0x%08x" vendorID),
      "  name        : "
        <> if Text.null adapterName
          then "(unknown)"
          else adapterName,
      "  description : "
        <> if Text.null driverDescription
          then "(unknown)"
          else driverDescription,
      "  type        : " <> adapterTypeTxt,
      "  backend     : " <> backendTypeTxt
    ]
  where
    adapterTypeTxt :: Text
    adapterTypeTxt = case adapterType of
      AdapterTypeDiscreteGPU -> "Discrete GPU"
      AdapterTypeIntegratedGPU -> "Integrated GPU"
      AdapterTypeCPU -> "CPU"
      AdapterTypeUnknown -> "(unknown)"

    backendTypeTxt :: Text
    backendTypeTxt = case backendType of
      BackendTypeNull -> "(unknown)"
      BackendTypeD3D11 -> "D3D 11"
      BackendTypeD3D12 -> "D3D 12"
      BackendTypeMetal -> "Metal"
      BackendTypeVulkan -> "Vulkan"
      BackendTypeOpenGL -> "OpenGL"
      BackendTypeOpenGLES -> "OpenGL ES"
