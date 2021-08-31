{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : WGPU.Internal.Multipurpose
-- Description : Multipurpose
--
-- This is a bit like a "Types" module. It exists to collect things which are
-- somewhat generic and are used in more than one part of the API.
module WGPU.Internal.Multipurpose
  ( -- * Types
    Texture (..),
    CompareFunction (..),
    Origin3D (..),
    Extent3D (..),
    TextureAspect (..),
    ImageCopyTexture (..),
    TextureDataLayout (..),
    IndexFormat (..),
  )
where

import Data.Word (Word32)
import Foreign (Word64, nullPtr)
import WGPU.Internal.Instance (Instance)
import WGPU.Internal.Memory (ToRaw, raw, showWithPtr)
import WGPU.Raw.Generated.Enum.WGPUCompareFunction (WGPUCompareFunction)
import qualified WGPU.Raw.Generated.Enum.WGPUCompareFunction as WGPUCompareFunction
import WGPU.Raw.Generated.Enum.WGPUIndexFormat (WGPUIndexFormat)
import qualified WGPU.Raw.Generated.Enum.WGPUIndexFormat as WGPUIndexFormat
import WGPU.Raw.Generated.Enum.WGPUTextureAspect (WGPUTextureAspect)
import qualified WGPU.Raw.Generated.Enum.WGPUTextureAspect as WGPUTextureAspect
import WGPU.Raw.Generated.Struct.WGPUExtent3D (WGPUExtent3D)
import qualified WGPU.Raw.Generated.Struct.WGPUExtent3D as WGPUExtent3D
import WGPU.Raw.Generated.Struct.WGPUImageCopyTexture (WGPUImageCopyTexture)
import qualified WGPU.Raw.Generated.Struct.WGPUImageCopyTexture as WGPU
import qualified WGPU.Raw.Generated.Struct.WGPUImageCopyTexture as WGPUImageCopyTexture
import WGPU.Raw.Generated.Struct.WGPUOrigin3D (WGPUOrigin3D)
import qualified WGPU.Raw.Generated.Struct.WGPUOrigin3D as WGPUOrigin3D
import WGPU.Raw.Generated.Struct.WGPUTextureDataLayout (WGPUTextureDataLayout)
import qualified WGPU.Raw.Generated.Struct.WGPUTextureDataLayout as WGPUTextureDataLayout
import WGPU.Raw.Types (WGPUTexture (WGPUTexture))

-------------------------------------------------------------------------------

-- | Handle to a texture.
data Texture = Texture
  { textureInst :: !Instance,
    wgpuTexture :: !WGPUTexture
  }

instance Show Texture where
  show t =
    let Texture _ (WGPUTexture ptr) = t
     in showWithPtr "Texture" ptr

instance Eq Texture where
  (==) t1 t2 =
    let Texture _ (WGPUTexture t1_ptr) = t1
        Texture _ (WGPUTexture t2_ptr) = t2
     in t1_ptr == t2_ptr

instance ToRaw Texture WGPUTexture where
  raw = pure . wgpuTexture

-------------------------------------------------------------------------------

-- | Comparison function used for depth and stencil operations.
data CompareFunction
  = CompareFunctionNever
  | CompareFunctionLess
  | CompareFunctionEqual
  | CompareFunctionLessEqual
  | CompareFunctionGreater
  | CompareFunctionNotEqual
  | CompareFunctionGreaterEqual
  | CompareFunctionAlways
  deriving (Eq, Show)

-- | Convert a 'CompareFunction' to its raw value.
instance ToRaw CompareFunction WGPUCompareFunction where
  raw cf =
    pure $
      case cf of
        CompareFunctionNever -> WGPUCompareFunction.Never
        CompareFunctionLess -> WGPUCompareFunction.Less
        CompareFunctionEqual -> WGPUCompareFunction.Equal
        CompareFunctionLessEqual -> WGPUCompareFunction.LessEqual
        CompareFunctionGreater -> WGPUCompareFunction.Greater
        CompareFunctionNotEqual -> WGPUCompareFunction.NotEqual
        CompareFunctionGreaterEqual -> WGPUCompareFunction.GreaterEqual
        CompareFunctionAlways -> WGPUCompareFunction.Always

-------------------------------------------------------------------------------

-- | Origin of a copy to/from a texture.
data Origin3D = Origin3D
  { originX :: !Word32,
    originY :: !Word32,
    originZ :: !Word32
  }
  deriving (Eq, Show)

instance ToRaw Origin3D WGPUOrigin3D where
  raw Origin3D {..} =
    pure $
      WGPUOrigin3D.WGPUOrigin3D
        { x = originX,
          y = originY,
          z = originZ
        }

-------------------------------------------------------------------------------

-- | Extent of a texture or texture-related operation.
data Extent3D = Extent3D
  { extentWidth :: !Word32,
    extentHeight :: !Word32,
    extentDepthOrArrayLayers :: !Word32
  }
  deriving (Eq, Show)

instance ToRaw Extent3D WGPUExtent3D where
  raw Extent3D {..} =
    pure $
      WGPUExtent3D.WGPUExtent3D
        { width = extentWidth,
          height = extentHeight,
          depthOrArrayLayers = extentDepthOrArrayLayers
        }

-------------------------------------------------------------------------------

-- | Kind of data a texture holds.
data TextureAspect
  = TextureAspectAll
  | TextureAspectStencilOnly
  | TextureAspectDepthOnly
  deriving (Eq, Show)

instance ToRaw TextureAspect WGPUTextureAspect where
  raw ta = pure $ case ta of
    TextureAspectAll -> WGPUTextureAspect.All
    TextureAspectStencilOnly -> WGPUTextureAspect.StencilOnly
    TextureAspectDepthOnly -> WGPUTextureAspect.DepthOnly

-------------------------------------------------------------------------------

-- | View of a texture which can be used to copy to/from a buffer/texture.
data ImageCopyTexture = ImageCopyTexture
  { texture :: !Texture,
    mipLevel :: !Word32,
    origin :: !Origin3D,
    aspect :: !TextureAspect
  }

instance ToRaw ImageCopyTexture WGPUImageCopyTexture where
  raw ImageCopyTexture {..} = do
    n_texture <- raw texture
    n_origin <- raw origin
    n_aspect <- raw aspect
    pure
      WGPUImageCopyTexture.WGPUImageCopyTexture
        { nextInChain = nullPtr,
          texture = n_texture,
          mipLevel = mipLevel,
          origin = n_origin,
          aspect = n_aspect
        }

-------------------------------------------------------------------------------

-- | Layout of a texture in a buffer's memory.
data TextureDataLayout = TextureDataLayout
  { -- | Offset into the buffer that is the start of the texture. Must be a
    -- multiple of texture block size. For non-compressed textures, this is 1.
    textureOffset :: !Word64,
    -- | Bytes per "row" in an image.
    bytesPerRow :: !Word32,
    -- | Rows that make up a single image. Used if there are multiple images.
    rowsPerImage :: !Word32
  }
  deriving (Eq, Show)

instance ToRaw TextureDataLayout WGPUTextureDataLayout where
  raw TextureDataLayout {..} =
    pure $
      WGPUTextureDataLayout.WGPUTextureDataLayout
        { nextInChain = nullPtr,
          offset = textureOffset,
          bytesPerRow = bytesPerRow,
          rowsPerImage = rowsPerImage
        }

-------------------------------------------------------------------------------

-- | Format of indices used within a pipeline.
data IndexFormat
  = -- | Indices are 16-bit unsigned integers ('Word16')
    IndexFormatUint16
  | -- | Indices are 32-bit unsigned integers ('Word32')
    IndexFormatUint32
  deriving (Eq, Show)

-- | Convert an 'IndexFormat' to its raw value.
instance ToRaw IndexFormat WGPUIndexFormat where
  raw idxFmt =
    pure $
      case idxFmt of
        IndexFormatUint16 -> WGPUIndexFormat.Uint16
        IndexFormatUint32 -> WGPUIndexFormat.Uint32
