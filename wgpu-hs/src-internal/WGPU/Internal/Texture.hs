{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

-- |
-- Module      : WGPU.Internal.Texture
-- Description : Textures and texture views.
module WGPU.Internal.Texture
  ( -- * Types
    TextureView (..),
    TextureFormat (..),
    TextureUsage (..),
    TextureViewDimension (..),
    TextureDimension (..),
    TextureDescriptor (..),
    TextureViewDescriptor (..),

    -- * Functions
    createTexture,
    createView,
    textureFormatFromRaw,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits ((.|.))
import Data.Default (Default, def)
import Data.Text (Text)
import Data.Word (Word32)
import Foreign (nullPtr)
import WGPU.Internal.Device (Device, deviceInst, wgpuDevice)
import WGPU.Internal.Instance (wgpuHsInstance)
import WGPU.Internal.Memory (ToRaw, evalContT, raw, rawPtr, showWithPtr)
import WGPU.Internal.Multipurpose
  ( Extent3D,
    Texture (Texture),
    TextureAspect,
    textureInst,
    wgpuTexture,
  )
import WGPU.Raw.Generated.Enum.WGPUTextureDimension (WGPUTextureDimension)
import qualified WGPU.Raw.Generated.Enum.WGPUTextureDimension as WGPUTextureDimension
import WGPU.Raw.Generated.Enum.WGPUTextureFormat (WGPUTextureFormat)
import qualified WGPU.Raw.Generated.Enum.WGPUTextureFormat as WGPUTextureFormat
import qualified WGPU.Raw.Generated.Enum.WGPUTextureUsage as WGPUTextureUsage
import WGPU.Raw.Generated.Enum.WGPUTextureViewDimension (WGPUTextureViewDimension)
import qualified WGPU.Raw.Generated.Enum.WGPUTextureViewDimension as WGPUTextureViewDimension
import qualified WGPU.Raw.Generated.Fun as RawFun
import WGPU.Raw.Generated.Struct.WGPUTextureDescriptor (WGPUTextureDescriptor)
import qualified WGPU.Raw.Generated.Struct.WGPUTextureDescriptor as WGPUTextureDescriptor
import WGPU.Raw.Generated.Struct.WGPUTextureViewDescriptor (WGPUTextureViewDescriptor)
import qualified WGPU.Raw.Generated.Struct.WGPUTextureViewDescriptor as WGPUTextureViewDescriptor
import WGPU.Raw.Types
  ( WGPUTextureUsageFlags,
    WGPUTextureView (WGPUTextureView),
  )

-------------------------------------------------------------------------------

-- | Handle to a texture view.
--
-- A 'TextureView' describes a texture and associated metadata needed by a
-- rendering pipeline or bind group.
newtype TextureView = TextureView {wgpuTextureView :: WGPUTextureView}

instance Show TextureView where
  show v =
    let TextureView (WGPUTextureView ptr) = v
     in showWithPtr "TextureView" ptr

instance Eq TextureView where
  (==) v1 v2 =
    let TextureView (WGPUTextureView v1_ptr) = v1
        TextureView (WGPUTextureView v2_ptr) = v2
     in v1_ptr == v2_ptr

instance ToRaw TextureView WGPUTextureView where
  raw = pure . wgpuTextureView

-------------------------------------------------------------------------------

-- | Dimensions of a particular texture view.
data TextureViewDimension
  = TextureViewDimension1D
  | TextureViewDimension2D
  | TextureViewDimension2DArray
  | TextureViewDimensionCube
  | TextureViewDimensionCubeArray
  | TextureViewDimension3D
  deriving (Eq, Show)

instance ToRaw TextureViewDimension WGPUTextureViewDimension where
  raw tvd =
    pure $
      case tvd of
        TextureViewDimension1D -> WGPUTextureViewDimension.D1D
        TextureViewDimension2D -> WGPUTextureViewDimension.D2D
        TextureViewDimension2DArray -> WGPUTextureViewDimension.D2DArray
        TextureViewDimensionCube -> WGPUTextureViewDimension.Cube
        TextureViewDimensionCubeArray -> WGPUTextureViewDimension.CubeArray
        TextureViewDimension3D -> WGPUTextureViewDimension.D3D

-------------------------------------------------------------------------------

-- | Different ways you can use a texture.
--
-- The usages determine from what kind of memory the texture is allocated, and
-- in what actions the texture can partake.
data TextureUsage = TextureUsage
  { texCopySrc :: !Bool,
    texCopyDst :: !Bool,
    texSampled :: !Bool,
    texStorage :: !Bool,
    texRenderAttachment :: !Bool
  }
  deriving (Eq, Show)

instance ToRaw TextureUsage WGPUTextureUsageFlags where
  raw TextureUsage {..} =
    pure $
      (if texCopySrc then WGPUTextureUsage.CopySrc else 0)
        .|. (if texCopyDst then WGPUTextureUsage.CopyDst else 0)
        .|. (if texSampled then WGPUTextureUsage.Sampled else 0)
        .|. (if texStorage then WGPUTextureUsage.Storage else 0)
        .|. (if texRenderAttachment then WGPUTextureUsage.RenderAttachment else 0)

instance Default TextureUsage where
  def =
    TextureUsage
      { texCopySrc = False,
        texCopyDst = False,
        texSampled = False,
        texStorage = False,
        texRenderAttachment = False
      }

-------------------------------------------------------------------------------

-- | Texture data format.
data TextureFormat
  = TextureFormatR8Unorm
  | TextureFormatR8Snorm
  | TextureFormatR8Uint
  | TextureFormatR8Sint
  | TextureFormatR16Uint
  | TextureFormatR16Sint
  | TextureFormatR16Float
  | TextureFormatRG8Unorm
  | TextureFormatRG8Snorm
  | TextureFormatRG8Uint
  | TextureFormatRG8Sint
  | TextureFormatR32Float
  | TextureFormatR32Uint
  | TextureFormatR32Sint
  | TextureFormatRG16Uint
  | TextureFormatRG16Sint
  | TextureFormatRG16Float
  | TextureFormatRGBA8Unorm
  | TextureFormatRGBA8UnormSrgb
  | TextureFormatRGBA8Snorm
  | TextureFormatRGBA8Uint
  | TextureFormatRGBA8Sint
  | TextureFormatBGRA8Unorm
  | TextureFormatBGRA8UnormSrgb
  | TextureFormatRGB10A2Unorm
  | TextureFormatRG11B10Ufloat
  | TextureFormatRGB9E5Ufloat
  | TextureFormatRG32Float
  | TextureFormatRG32Uint
  | TextureFormatRG32Sint
  | TextureFormatRGBA16Uint
  | TextureFormatRGBA16Sint
  | TextureFormatRGBA16Float
  | TextureFormatRGBA32Float
  | TextureFormatRGBA32Uint
  | TextureFormatRGBA32Sint
  | TextureFormatDepth32Float
  | TextureFormatDepth24Plus
  | TextureFormatDepth24PlusStencil8
  | TextureFormatStencil8
  | TextureFormatBC1RGBAUnorm
  | TextureFormatBC1RGBAUnormSrgb
  | TextureFormatBC2RGBAUnorm
  | TextureFormatBC2RGBAUnormSrgb
  | TextureFormatBC3RGBAUnorm
  | TextureFormatBC3RGBAUnormSrgb
  | TextureFormatBC4RUnorm
  | TextureFormatBC4RSnorm
  | TextureFormatBC5RGUnorm
  | TextureFormatBC5RGSnorm
  | TextureFormatBC6HRGBUfloat
  | TextureFormatBC6HRGBFloat
  | TextureFormatBC7RGBAUnorm
  | TextureFormatBC7RGBAUnormSrgb
  deriving (Eq, Show)

instance ToRaw TextureFormat WGPUTextureFormat where
  raw tf =
    pure $
      case tf of
        TextureFormatR8Unorm -> WGPUTextureFormat.R8Unorm
        TextureFormatR8Snorm -> WGPUTextureFormat.R8Snorm
        TextureFormatR8Uint -> WGPUTextureFormat.R8Uint
        TextureFormatR8Sint -> WGPUTextureFormat.R8Sint
        TextureFormatR16Uint -> WGPUTextureFormat.R16Uint
        TextureFormatR16Sint -> WGPUTextureFormat.R16Sint
        TextureFormatR16Float -> WGPUTextureFormat.R16Float
        TextureFormatRG8Unorm -> WGPUTextureFormat.RG8Unorm
        TextureFormatRG8Snorm -> WGPUTextureFormat.RG8Snorm
        TextureFormatRG8Uint -> WGPUTextureFormat.RG8Uint
        TextureFormatRG8Sint -> WGPUTextureFormat.RG8Sint
        TextureFormatR32Float -> WGPUTextureFormat.R32Float
        TextureFormatR32Uint -> WGPUTextureFormat.R32Uint
        TextureFormatR32Sint -> WGPUTextureFormat.R32Sint
        TextureFormatRG16Uint -> WGPUTextureFormat.RG16Uint
        TextureFormatRG16Sint -> WGPUTextureFormat.RG16Sint
        TextureFormatRG16Float -> WGPUTextureFormat.RG16Float
        TextureFormatRGBA8Unorm -> WGPUTextureFormat.RGBA8Unorm
        TextureFormatRGBA8UnormSrgb -> WGPUTextureFormat.RGBA8UnormSrgb
        TextureFormatRGBA8Snorm -> WGPUTextureFormat.RGBA8Snorm
        TextureFormatRGBA8Uint -> WGPUTextureFormat.RGBA8Uint
        TextureFormatRGBA8Sint -> WGPUTextureFormat.RGBA8Sint
        TextureFormatBGRA8Unorm -> WGPUTextureFormat.BGRA8Unorm
        TextureFormatBGRA8UnormSrgb -> WGPUTextureFormat.BGRA8UnormSrgb
        TextureFormatRGB10A2Unorm -> WGPUTextureFormat.RGB10A2Unorm
        TextureFormatRG11B10Ufloat -> WGPUTextureFormat.RG11B10Ufloat
        TextureFormatRGB9E5Ufloat -> WGPUTextureFormat.RGB9E5Ufloat
        TextureFormatRG32Float -> WGPUTextureFormat.RG32Float
        TextureFormatRG32Uint -> WGPUTextureFormat.RG32Uint
        TextureFormatRG32Sint -> WGPUTextureFormat.RG32Sint
        TextureFormatRGBA16Uint -> WGPUTextureFormat.RGBA16Uint
        TextureFormatRGBA16Sint -> WGPUTextureFormat.RGBA16Sint
        TextureFormatRGBA16Float -> WGPUTextureFormat.RGBA16Float
        TextureFormatRGBA32Float -> WGPUTextureFormat.RGBA32Float
        TextureFormatRGBA32Uint -> WGPUTextureFormat.RGBA32Uint
        TextureFormatRGBA32Sint -> WGPUTextureFormat.RGBA32Sint
        TextureFormatDepth32Float -> WGPUTextureFormat.Depth32Float
        TextureFormatDepth24Plus -> WGPUTextureFormat.Depth24Plus
        TextureFormatDepth24PlusStencil8 ->
          WGPUTextureFormat.Depth24PlusStencil8
        TextureFormatStencil8 -> WGPUTextureFormat.Stencil8
        TextureFormatBC1RGBAUnorm -> WGPUTextureFormat.BC1RGBAUnorm
        TextureFormatBC1RGBAUnormSrgb -> WGPUTextureFormat.BC1RGBAUnormSrgb
        TextureFormatBC2RGBAUnorm -> WGPUTextureFormat.BC2RGBAUnorm
        TextureFormatBC2RGBAUnormSrgb -> WGPUTextureFormat.BC2RGBAUnormSrgb
        TextureFormatBC3RGBAUnorm -> WGPUTextureFormat.BC3RGBAUnorm
        TextureFormatBC3RGBAUnormSrgb -> WGPUTextureFormat.BC3RGBAUnormSrgb
        TextureFormatBC4RUnorm -> WGPUTextureFormat.BC4RUnorm
        TextureFormatBC4RSnorm -> WGPUTextureFormat.BC4RSnorm
        TextureFormatBC5RGUnorm -> WGPUTextureFormat.BC5RGUnorm
        TextureFormatBC5RGSnorm -> WGPUTextureFormat.BC5RGSnorm
        TextureFormatBC6HRGBUfloat -> WGPUTextureFormat.BC6HRGBUfloat
        TextureFormatBC6HRGBFloat -> WGPUTextureFormat.BC6HRGBFloat
        TextureFormatBC7RGBAUnorm -> WGPUTextureFormat.BC7RGBAUnorm
        TextureFormatBC7RGBAUnormSrgb -> WGPUTextureFormat.BC7RGBAUnormSrgb

textureFormatFromRaw :: WGPUTextureFormat -> TextureFormat
textureFormatFromRaw rt =
  case rt of
    WGPUTextureFormat.R8Unorm -> TextureFormatR8Unorm
    WGPUTextureFormat.R8Snorm -> TextureFormatR8Snorm
    WGPUTextureFormat.R8Uint -> TextureFormatR8Uint
    WGPUTextureFormat.R8Sint -> TextureFormatR8Sint
    WGPUTextureFormat.R16Uint -> TextureFormatR16Uint
    WGPUTextureFormat.R16Sint -> TextureFormatR16Sint
    WGPUTextureFormat.R16Float -> TextureFormatR16Float
    WGPUTextureFormat.RG8Unorm -> TextureFormatRG8Unorm
    WGPUTextureFormat.RG8Snorm -> TextureFormatRG8Snorm
    WGPUTextureFormat.RG8Uint -> TextureFormatRG8Uint
    WGPUTextureFormat.RG8Sint -> TextureFormatRG8Sint
    WGPUTextureFormat.R32Float -> TextureFormatR32Float
    WGPUTextureFormat.R32Uint -> TextureFormatR32Uint
    WGPUTextureFormat.R32Sint -> TextureFormatR32Sint
    WGPUTextureFormat.RG16Uint -> TextureFormatRG16Uint
    WGPUTextureFormat.RG16Sint -> TextureFormatRG16Sint
    WGPUTextureFormat.RG16Float -> TextureFormatRG16Float
    WGPUTextureFormat.RGBA8Unorm -> TextureFormatRGBA8Unorm
    WGPUTextureFormat.RGBA8UnormSrgb -> TextureFormatRGBA8UnormSrgb
    WGPUTextureFormat.RGBA8Snorm -> TextureFormatRGBA8Snorm
    WGPUTextureFormat.RGBA8Uint -> TextureFormatRGBA8Uint
    WGPUTextureFormat.RGBA8Sint -> TextureFormatRGBA8Sint
    WGPUTextureFormat.BGRA8Unorm -> TextureFormatBGRA8Unorm
    WGPUTextureFormat.BGRA8UnormSrgb -> TextureFormatBGRA8UnormSrgb
    WGPUTextureFormat.RGB10A2Unorm -> TextureFormatRGB10A2Unorm
    WGPUTextureFormat.RG11B10Ufloat -> TextureFormatRG11B10Ufloat
    WGPUTextureFormat.RGB9E5Ufloat -> TextureFormatRGB9E5Ufloat
    WGPUTextureFormat.RG32Float -> TextureFormatRG32Float
    WGPUTextureFormat.RG32Uint -> TextureFormatRG32Uint
    WGPUTextureFormat.RG32Sint -> TextureFormatRG32Sint
    WGPUTextureFormat.RGBA16Uint -> TextureFormatRGBA16Uint
    WGPUTextureFormat.RGBA16Sint -> TextureFormatRGBA16Sint
    WGPUTextureFormat.RGBA16Float -> TextureFormatRGBA16Float
    WGPUTextureFormat.RGBA32Float -> TextureFormatRGBA32Float
    WGPUTextureFormat.RGBA32Uint -> TextureFormatRGBA32Uint
    WGPUTextureFormat.RGBA32Sint -> TextureFormatRGBA32Sint
    WGPUTextureFormat.Depth32Float -> TextureFormatDepth32Float
    WGPUTextureFormat.Depth24Plus -> TextureFormatDepth24Plus
    WGPUTextureFormat.Depth24PlusStencil8 -> TextureFormatDepth24PlusStencil8
    WGPUTextureFormat.Stencil8 -> TextureFormatStencil8
    WGPUTextureFormat.BC1RGBAUnorm -> TextureFormatBC1RGBAUnorm
    WGPUTextureFormat.BC1RGBAUnormSrgb -> TextureFormatBC1RGBAUnormSrgb
    WGPUTextureFormat.BC2RGBAUnorm -> TextureFormatBC2RGBAUnorm
    WGPUTextureFormat.BC2RGBAUnormSrgb -> TextureFormatBC2RGBAUnormSrgb
    WGPUTextureFormat.BC3RGBAUnorm -> TextureFormatBC3RGBAUnorm
    WGPUTextureFormat.BC3RGBAUnormSrgb -> TextureFormatBC3RGBAUnormSrgb
    WGPUTextureFormat.BC4RUnorm -> TextureFormatBC4RUnorm
    WGPUTextureFormat.BC4RSnorm -> TextureFormatBC4RSnorm
    WGPUTextureFormat.BC5RGUnorm -> TextureFormatBC5RGUnorm
    WGPUTextureFormat.BC5RGSnorm -> TextureFormatBC5RGSnorm
    WGPUTextureFormat.BC6HRGBUfloat -> TextureFormatBC6HRGBUfloat
    WGPUTextureFormat.BC6HRGBFloat -> TextureFormatBC6HRGBFloat
    WGPUTextureFormat.BC7RGBAUnorm -> TextureFormatBC7RGBAUnorm
    WGPUTextureFormat.BC7RGBAUnormSrgb -> TextureFormatBC7RGBAUnormSrgb
    _ -> error $ "Unexpected WGPUTextureFormat" <> show rt

-------------------------------------------------------------------------------

-- | Dimensionality of a texture.
data TextureDimension
  = TextureDimension1D
  | TextureDimension2D
  | TextureDimension3D
  deriving (Eq, Show)

instance ToRaw TextureDimension WGPUTextureDimension where
  raw td = pure $
    case td of
      TextureDimension1D -> WGPUTextureDimension.D1D
      TextureDimension2D -> WGPUTextureDimension.D2D
      TextureDimension3D -> WGPUTextureDimension.D3D

-------------------------------------------------------------------------------

-- | Describes a 'Texture'.
data TextureDescriptor = TextureDescriptor
  { textureLabel :: !Text,
    textureSize :: !Extent3D,
    mipLevelCount :: !Word32,
    sampleCount :: !Word32,
    dimension :: !TextureDimension,
    format :: !TextureFormat,
    textureUsage :: !TextureUsage
  }
  deriving (Eq, Show)

instance ToRaw TextureDescriptor WGPUTextureDescriptor where
  raw TextureDescriptor {..} = do
    label_ptr <- rawPtr textureLabel
    n_usage <- raw textureUsage
    n_dimension <- raw dimension
    n_size <- raw textureSize
    n_format <- raw format
    pure $
      WGPUTextureDescriptor.WGPUTextureDescriptor
        { nextInChain = nullPtr,
          label = label_ptr,
          usage = n_usage,
          dimension = n_dimension,
          size = n_size,
          format = n_format,
          mipLevelCount = mipLevelCount,
          sampleCount = sampleCount
        }

-------------------------------------------------------------------------------

-- | Describes a 'TextureView'.
data TextureViewDescriptor = TextureViewDescriptor
  { textureViewLabel :: !Text,
    textureViewFormat :: !TextureFormat,
    textureViewDimension :: !TextureViewDimension,
    textureViewBaseMipLevel :: !Word32,
    textureViewMipLevelCount :: !Word32,
    baseArrayLayer :: !Word32,
    arrayLayerCount :: !Word32,
    textureViewAspect :: !TextureAspect
  }
  deriving (Eq, Show)

instance ToRaw TextureViewDescriptor WGPUTextureViewDescriptor where
  raw TextureViewDescriptor {..} = do
    label_ptr <- rawPtr textureViewLabel
    n_format <- raw textureViewFormat
    n_dimension <- raw textureViewDimension
    n_aspect <- raw textureViewAspect
    pure $
      WGPUTextureViewDescriptor.WGPUTextureViewDescriptor
        { nextInChain = nullPtr,
          label = label_ptr,
          format = n_format,
          dimension = n_dimension,
          baseMipLevel = textureViewBaseMipLevel,
          mipLevelCount = textureViewMipLevelCount,
          baseArrayLayer = baseArrayLayer,
          arrayLayerCount = arrayLayerCount,
          aspect = n_aspect
        }

-------------------------------------------------------------------------------

-- | Create a texture.
createTexture ::
  MonadIO m =>
  -- | Device for which to create the texture.
  Device ->
  -- | Description of the texture to create.
  TextureDescriptor ->
  -- | Action to create the texture.
  m Texture
createTexture device textureDescriptor = liftIO . evalContT $ do
  let inst = deviceInst device
  textureDescriptor_ptr <- rawPtr textureDescriptor
  Texture inst
    <$> RawFun.wgpuDeviceCreateTexture
      (wgpuHsInstance inst)
      (wgpuDevice device)
      textureDescriptor_ptr

-- | Create a view of a texture.
createView ::
  MonadIO m =>
  -- | Texture for which the view should be created.
  Texture ->
  -- | Description of the texture view.
  TextureViewDescriptor ->
  -- | Created texture view.
  m TextureView
createView texture textureViewDescriptor = liftIO . evalContT $ do
  let inst = textureInst texture
  textureViewDescriptor_ptr <- rawPtr textureViewDescriptor
  TextureView
    <$> RawFun.wgpuTextureCreateView
      (wgpuHsInstance inst)
      (wgpuTexture texture)
      textureViewDescriptor_ptr
