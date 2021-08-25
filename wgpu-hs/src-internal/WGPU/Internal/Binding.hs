{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module      : WGPU.Internal.Binding
-- Description : Resource Binding
module WGPU.Internal.Binding
  ( -- * Types
    BindGroupLayout (..),
    BindGroupLayoutDescriptor (..),
    BindGroupLayoutEntry (..),
    Binding (..),
    ShaderStage (..),
    BindingType (..),
    BufferBindingLayout (..),
    SamplerBindingLayout (..),
    TextureBindingLayout (..),
    StorageTextureBindingLayout (..),
    StorageTextureAccess (..),
    TextureSampleType (..),
    BufferBindingType (..),

    -- * Functions
    createBindGroupLayout,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Cont (evalContT)
import Data.Bits ((.|.))
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word32, Word64)
import Foreign (nullPtr)
import Foreign.C (CBool (CBool))
import WGPU.Internal.Device (Device, deviceInst, wgpuDevice)
import WGPU.Internal.Instance (wgpuHsInstance)
import WGPU.Internal.Memory (ToRaw, raw, rawArrayPtr, rawPtr, showWithPtr)
import WGPU.Internal.SMaybe (SMaybe, fromSMaybe)
import WGPU.Internal.Texture (TextureFormat, TextureViewDimension)
import WGPU.Raw.Generated.Enum.WGPUBufferBindingType (WGPUBufferBindingType)
import qualified WGPU.Raw.Generated.Enum.WGPUBufferBindingType as WGPUBufferBindingType
import qualified WGPU.Raw.Generated.Enum.WGPUSamplerBindingType as WGPUSamplerBindingType
import qualified WGPU.Raw.Generated.Enum.WGPUShaderStage as WGPUShaderStage
import WGPU.Raw.Generated.Enum.WGPUStorageTextureAccess (WGPUStorageTextureAccess)
import qualified WGPU.Raw.Generated.Enum.WGPUStorageTextureAccess as WGPUStorageTextureAccess
import qualified WGPU.Raw.Generated.Enum.WGPUTextureFormat as WGPUTextureFormat
import WGPU.Raw.Generated.Enum.WGPUTextureSampleType (WGPUTextureSampleType)
import qualified WGPU.Raw.Generated.Enum.WGPUTextureSampleType as WGPUTextureSampleType
import qualified WGPU.Raw.Generated.Enum.WGPUTextureViewDimension as WGPUTextureViewDimension
import qualified WGPU.Raw.Generated.Fun as RawFun
import WGPU.Raw.Generated.Struct.WGPUBindGroupLayoutDescriptor (WGPUBindGroupLayoutDescriptor)
import qualified WGPU.Raw.Generated.Struct.WGPUBindGroupLayoutDescriptor as WGPUBindGroupLayoutDescriptor
import WGPU.Raw.Generated.Struct.WGPUBindGroupLayoutEntry (WGPUBindGroupLayoutEntry)
import qualified WGPU.Raw.Generated.Struct.WGPUBindGroupLayoutEntry as WGPUBindGroupLayoutEntry
import WGPU.Raw.Generated.Struct.WGPUBufferBindingLayout (WGPUBufferBindingLayout)
import qualified WGPU.Raw.Generated.Struct.WGPUBufferBindingLayout as WGPUBufferBindingLayout
import WGPU.Raw.Generated.Struct.WGPUSamplerBindingLayout (WGPUSamplerBindingLayout)
import qualified WGPU.Raw.Generated.Struct.WGPUSamplerBindingLayout as WGPUSamplerBindingLayout
import WGPU.Raw.Generated.Struct.WGPUStorageTextureBindingLayout (WGPUStorageTextureBindingLayout)
import qualified WGPU.Raw.Generated.Struct.WGPUStorageTextureBindingLayout as WGPUStorageTextureBindingLayout
import WGPU.Raw.Generated.Struct.WGPUTextureBindingLayout (WGPUTextureBindingLayout)
import qualified WGPU.Raw.Generated.Struct.WGPUTextureBindingLayout as WGPUTextureBindingLayout
import WGPU.Raw.Types
  ( WGPUBindGroupLayout (WGPUBindGroupLayout),
    WGPUShaderStageFlags,
  )

-------------------------------------------------------------------------------

-- | Handle to a binding group layout.
--
-- A @BindGroupLayout@ is a handle to the GPU-side layout of a binding group.
newtype BindGroupLayout = BindGroupLayout {wgpuBindGroupLayout :: WGPUBindGroupLayout}

instance Show BindGroupLayout where
  show b =
    let BindGroupLayout (WGPUBindGroupLayout ptr) = b
     in showWithPtr "BindGroupLayout" ptr

instance Eq BindGroupLayout where
  (==) b1 b2 =
    let BindGroupLayout (WGPUBindGroupLayout b1_ptr) = b1
        BindGroupLayout (WGPUBindGroupLayout b2_ptr) = b2
     in b1_ptr == b2_ptr

instance ToRaw BindGroupLayout WGPUBindGroupLayout where
  raw = pure . wgpuBindGroupLayout

-------------------------------------------------------------------------------

-- | Creates a 'BindGroupLayout'.
createBindGroupLayout ::
  MonadIO m =>
  -- | The device for which the bind group layout will be created.
  Device ->
  -- | Description of the bind group layout.
  BindGroupLayoutDescriptor ->
  -- | MonadIO action that creates a bind group layout.
  m BindGroupLayout
createBindGroupLayout device ld = liftIO . evalContT $ do
  let inst = deviceInst device

  bindGroupLayoutDescriptor_ptr <- rawPtr ld
  rawBindGroupLayout <-
    RawFun.wgpuDeviceCreateBindGroupLayout
      (wgpuHsInstance inst)
      (wgpuDevice device)
      bindGroupLayoutDescriptor_ptr
  pure (BindGroupLayout rawBindGroupLayout)

-------------------------------------------------------------------------------

-- | Describes a 'BindGroupLayout'.
data BindGroupLayoutDescriptor = BindGroupLayoutDescriptor
  { -- | Debug label of the bind group layout.
    bindGroupLabel :: !Text,
    -- | Sequence of entries in this bind group layout.
    entries :: !(Vector BindGroupLayoutEntry)
  }
  deriving (Eq, Show)

instance ToRaw BindGroupLayoutDescriptor WGPUBindGroupLayoutDescriptor where
  raw BindGroupLayoutDescriptor {..} = do
    label_ptr <- rawPtr bindGroupLabel
    let n_entryCount = fromIntegral . length $ entries
    entries_ptr <- rawArrayPtr entries
    pure
      WGPUBindGroupLayoutDescriptor.WGPUBindGroupLayoutDescriptor
        { nextInChain = nullPtr,
          label = label_ptr,
          entryCount = n_entryCount,
          entries = entries_ptr
        }

-------------------------------------------------------------------------------

-- | Describes a single binding inside a bind group.
data BindGroupLayoutEntry = BindGroupLayoutEntry
  { -- | Binding index. Must match a shader index, and be unique inside a
    --   bind group layout.
    binding :: !Binding,
    -- | Which shader stages can see this binding.
    visibility :: !ShaderStage,
    -- | Type of the binding.
    bindGroupLayoutEntryType :: !BindingType
  }
  deriving (Eq, Show)

instance ToRaw BindGroupLayoutEntry WGPUBindGroupLayoutEntry where
  raw BindGroupLayoutEntry {..} = do
    n_binding <- raw binding
    n_visibility <- raw visibility
    (n_buffer, n_sampler, n_texture, n_storageTexture) <-
      case bindGroupLayoutEntryType of
        BindingTypeBuffer bbl -> do
          nn_buffer <- raw bbl
          pure (nn_buffer, noSampler, noTexture, noStorageTexture)
        BindingTypeSampler sbl -> do
          nn_sampler <- raw sbl
          pure (noBuffer, nn_sampler, noTexture, noStorageTexture)
        BindingTypeTexture tbl -> do
          nn_texture <- raw tbl
          pure (noBuffer, noSampler, nn_texture, noStorageTexture)
        BindingTypeStorageTexture stbl -> do
          nn_storageTexture <- raw stbl
          pure (noBuffer, noSampler, noTexture, nn_storageTexture)
    pure
      WGPUBindGroupLayoutEntry.WGPUBindGroupLayoutEntry
        { nextInChain = nullPtr,
          binding = n_binding,
          visibility = n_visibility,
          buffer = n_buffer,
          sampler = n_sampler,
          texture = n_texture,
          storageTexture = n_storageTexture
        }
    where
      noBuffer :: WGPUBufferBindingLayout
      noBuffer =
        WGPUBufferBindingLayout.WGPUBufferBindingLayout
          { nextInChain = nullPtr,
            typ = WGPUBufferBindingType.Undefined,
            hasDynamicOffset = CBool 0,
            minBindingSize = 0
          }

      noSampler :: WGPUSamplerBindingLayout
      noSampler =
        WGPUSamplerBindingLayout.WGPUSamplerBindingLayout
          { nextInChain = nullPtr,
            typ = WGPUSamplerBindingType.Undefined
          }

      noTexture :: WGPUTextureBindingLayout
      noTexture =
        WGPUTextureBindingLayout.WGPUTextureBindingLayout
          { nextInChain = nullPtr,
            sampleType = WGPUTextureSampleType.Undefined,
            viewDimension = WGPUTextureViewDimension.Undefined,
            multisampled = CBool 0
          }

      noStorageTexture :: WGPUStorageTextureBindingLayout
      noStorageTexture =
        WGPUStorageTextureBindingLayout.WGPUStorageTextureBindingLayout
          { nextInChain = nullPtr,
            access = WGPUStorageTextureAccess.Undefined,
            format = WGPUTextureFormat.Undefined,
            viewDimension = WGPUTextureViewDimension.Undefined
          }

-------------------------------------------------------------------------------

-- | Binding index.
--
-- This must match a shader index, and be unique inside a binding group
-- layout.
newtype Binding = Binding {unBinding :: Word32} deriving (Eq, Show)

instance ToRaw Binding Word32 where
  raw = pure . unBinding

-------------------------------------------------------------------------------

-- | Describes the shader stages from which a binding will be visible.
data ShaderStage = ShaderStage
  { -- | Binding is visible from the vertex shader of a render pipeline.
    stageVertex :: !Bool,
    -- | Binding is visible from the fragment shader of a render pipeline.
    stageFragment :: !Bool,
    -- | Binding is visible from the compute shader of a compute pipeline.
    stageCompute :: !Bool
  }
  deriving (Eq, Show)

instance ToRaw ShaderStage WGPUShaderStageFlags where
  raw ShaderStage {..} =
    pure $
      (if stageVertex then WGPUShaderStage.Vertex else 0)
        .|. (if stageFragment then WGPUShaderStage.Fragment else 0)
        .|. (if stageCompute then WGPUShaderStage.Compute else 0)

-------------------------------------------------------------------------------

-- | Specifies type of a binding.
data BindingType
  = -- | A buffer binding.
    BindingTypeBuffer !BufferBindingLayout
  | -- | A sampler that can be used to sample a texture.
    BindingTypeSampler !SamplerBindingLayout
  | -- | A texture binding.
    BindingTypeTexture !TextureBindingLayout
  | -- | A storage texture.
    BindingTypeStorageTexture !StorageTextureBindingLayout
  deriving (Eq, Show)

-------------------------------------------------------------------------------

-- | A buffer binding.
data BufferBindingLayout = BufferBindingLayout
  { -- | Sub-type of the buffer binding.
    bindingBufferLayoutType :: !BufferBindingType,
    -- | Indicates that the binding has a dynamic offset. One offset must be
    --   passed when setting the bind group in the render pass.
    hasDynamicOffset :: !Bool,
    -- | Minimum size of a corresponding buffer binding required to match this
    --   entry.
    minBindingSize :: !(SMaybe Word64)
  }
  deriving (Eq, Show)

instance ToRaw BufferBindingLayout WGPUBufferBindingLayout where
  raw BufferBindingLayout {..} = do
    n_typ <- raw bindingBufferLayoutType
    n_hasDynamicOffset <- raw hasDynamicOffset
    pure
      WGPUBufferBindingLayout.WGPUBufferBindingLayout
        { nextInChain = nullPtr,
          typ = n_typ,
          hasDynamicOffset = n_hasDynamicOffset,
          minBindingSize = fromSMaybe 0 minBindingSize
        }

-------------------------------------------------------------------------------

-- | A sampler binding that can be used to sample a texture.
data SamplerBindingLayout
  = SamplerBindingLayoutFiltering
  | SamplerBindingLayoutNonFiltering
  | SamplerBindingLayoutComparison
  deriving (Eq, Show)

instance ToRaw SamplerBindingLayout WGPUSamplerBindingLayout where
  raw sbl =
    pure $
      WGPUSamplerBindingLayout.WGPUSamplerBindingLayout
        { nextInChain = nullPtr,
          typ =
            case sbl of
              SamplerBindingLayoutFiltering ->
                WGPUSamplerBindingType.Filtering
              SamplerBindingLayoutNonFiltering ->
                WGPUSamplerBindingType.NonFiltering
              SamplerBindingLayoutComparison ->
                WGPUSamplerBindingType.Comparison
        }

-------------------------------------------------------------------------------

-- | A texture binding.
data TextureBindingLayout = TextureBindingLayout
  { -- | Sample type of the texture binding.
    sampleType :: !TextureSampleType,
    -- | Dimension of the texture view that is going to be sampled.
    textureViewDimension :: !TextureViewDimension,
    -- | True if the texture has a sample count greater than 1.
    multiSampled :: !Bool
  }
  deriving (Eq, Show)

instance ToRaw TextureBindingLayout WGPUTextureBindingLayout where
  raw TextureBindingLayout {..} = do
    n_sampleType <- raw sampleType
    n_viewDimension <- raw textureViewDimension
    n_multisampled <- raw multiSampled
    pure
      WGPUTextureBindingLayout.WGPUTextureBindingLayout
        { nextInChain = nullPtr,
          sampleType = n_sampleType,
          viewDimension = n_viewDimension,
          multisampled = n_multisampled
        }

-------------------------------------------------------------------------------

-- | A storage texture binding.
data StorageTextureBindingLayout = StorageTextureBindingLayout
  { -- | Permitted access to this texture.
    access :: !StorageTextureAccess,
    -- | Format of the texture.
    storageTextureFormat :: !TextureFormat,
    -- | Dimension of the texture view that is going to be sampled.
    storageTextureViewDimension :: !TextureViewDimension
  }
  deriving (Eq, Show)

instance ToRaw StorageTextureBindingLayout WGPUStorageTextureBindingLayout where
  raw StorageTextureBindingLayout {..} = do
    n_access <- raw access
    n_format <- raw storageTextureFormat
    n_viewDimension <- raw storageTextureViewDimension
    pure
      WGPUStorageTextureBindingLayout.WGPUStorageTextureBindingLayout
        { nextInChain = nullPtr,
          access = n_access,
          format = n_format,
          viewDimension = n_viewDimension
        }

-------------------------------------------------------------------------------

-- | Specific method of allowed access to a storage texture.
data StorageTextureAccess
  = StorageTextureAccessReadOnly
  | StorageTextureAccessWriteOnly
  | StorageTextureAccessReadWrite
  deriving (Eq, Show)

instance ToRaw StorageTextureAccess WGPUStorageTextureAccess where
  raw sta =
    pure $
      case sta of
        StorageTextureAccessReadOnly -> WGPUStorageTextureAccess.ReadOnly
        StorageTextureAccessWriteOnly -> WGPUStorageTextureAccess.WriteOnly
        StorageTextureAccessReadWrite -> WGPUStorageTextureAccess.Undefined -- ?

-------------------------------------------------------------------------------

-- | Specific type of a sample in a texture binding.
data TextureSampleType
  = TextureSampleTypeFloat {filterable :: !Bool}
  | TextureSampleTypeDepth
  | TextureSampleTypeSignedInt
  | TextureSampleTypeUnsignedInt
  deriving (Eq, Show)

instance ToRaw TextureSampleType WGPUTextureSampleType where
  raw tt =
    pure $
      case tt of
        TextureSampleTypeFloat False -> WGPUTextureSampleType.UnfilterableFloat
        TextureSampleTypeFloat True -> WGPUTextureSampleType.Float
        TextureSampleTypeDepth -> WGPUTextureSampleType.Depth
        TextureSampleTypeSignedInt -> WGPUTextureSampleType.Sint
        TextureSampleTypeUnsignedInt -> WGPUTextureSampleType.Uint

-------------------------------------------------------------------------------

-- | Specific type of a buffer binding.
data BufferBindingType
  = Uniform
  | Storage {readOnly :: !Bool}
  deriving (Eq, Show)

instance ToRaw BufferBindingType WGPUBufferBindingType where
  raw bt =
    pure $
      case bt of
        Uniform -> WGPUBufferBindingType.Uniform
        Storage False -> WGPUBufferBindingType.Storage
        Storage True -> WGPUBufferBindingType.ReadOnlyStorage
