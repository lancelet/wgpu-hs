{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

-- |
-- Module      : WGPU.Internal.Sampler
-- Description : Texture sampling.
module WGPU.Internal.Sampler
  ( -- * Types
    Sampler (..),
    AddressMode (..),
    FilterMode (..),
    SamplerDescriptor (..),

    -- * Functions
    createSampler,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Word (Word16)
import Foreign (nullPtr)
import Foreign.C (CFloat (CFloat))
import WGPU.Internal.Device (Device, deviceInst, wgpuDevice)
import WGPU.Internal.Instance (wgpuHsInstance)
import WGPU.Internal.Memory (ToRaw, evalContT, raw, rawPtr, showWithPtr)
import WGPU.Internal.Multipurpose (CompareFunction)
import WGPU.Raw.Generated.Enum.WGPUAddressMode (WGPUAddressMode)
import qualified WGPU.Raw.Generated.Enum.WGPUAddressMode as WGPUAddressMode
import qualified WGPU.Raw.Generated.Enum.WGPUAddressMode as WPUAddressMode
import WGPU.Raw.Generated.Enum.WGPUFilterMode (WGPUFilterMode)
import qualified WGPU.Raw.Generated.Enum.WGPUFilterMode as WGPUFilterMode
import qualified WGPU.Raw.Generated.Fun as RawFun
import WGPU.Raw.Generated.Struct.WGPUSamplerDescriptor (WGPUSamplerDescriptor)
import qualified WGPU.Raw.Generated.Struct.WGPUSamplerDescriptor as WGPUSamplerDescriptor
import WGPU.Raw.Types (WGPUSampler (WGPUSampler))

-------------------------------------------------------------------------------

-- | Handle to a 'Sampler'.
--
-- A 'Sampler' defines how a pipeline will sample from a 'TextureView'.
-- Samplers define image filters (include anisotropy) and address (wrapping)
-- modes, among other things.
newtype Sampler = Sampler {wgpuSampler :: WGPUSampler}

instance Show Sampler where
  show s =
    let Sampler (WGPUSampler ptr) = s
     in showWithPtr "Sampler" ptr

instance Eq Sampler where
  (==) s1 s2 =
    let Sampler (WGPUSampler s1_ptr) = s1
        Sampler (WGPUSampler s2_ptr) = s2
     in s1_ptr == s2_ptr

instance ToRaw Sampler WGPUSampler where
  raw = pure . wgpuSampler

-------------------------------------------------------------------------------

-- | How edges should be handled in texture addressing.
data AddressMode
  = AddressModeClampToEdge
  | AddressModeRepeat
  | AddressModeMirrorRepeat
  deriving (Eq, Show)

instance ToRaw AddressMode WGPUAddressMode where
  raw am =
    pure $ case am of
      AddressModeClampToEdge -> WPUAddressMode.ClampToEdge
      AddressModeRepeat -> WGPUAddressMode.Repeat
      AddressModeMirrorRepeat -> WGPUAddressMode.MirrorRepeat

-------------------------------------------------------------------------------

-- | Texel mixing mode when sampling between texels.
data FilterMode
  = FilterModeNearest
  | FilterModeLinear
  deriving (Eq, Show)

instance ToRaw FilterMode WGPUFilterMode where
  raw fm =
    pure $ case fm of
      FilterModeNearest -> WGPUFilterMode.Nearest
      FilterModeLinear -> WGPUFilterMode.Linear

-------------------------------------------------------------------------------

-- | Describes a 'Sampler'.
data SamplerDescriptor = SamplerDescriptor
  { samplerLabel :: !Text,
    addressModeU :: !AddressMode,
    addressModeV :: !AddressMode,
    addressModeW :: !AddressMode,
    magFilter :: !FilterMode,
    minFilter :: !FilterMode,
    mipmapFilter :: !FilterMode,
    lodMinClamp :: !Float,
    lodMaxClamp :: !Float,
    samplerCompare :: !CompareFunction,
    maxAnisotropy :: !Word16
  }
  deriving (Eq, Show)

instance ToRaw SamplerDescriptor WGPUSamplerDescriptor where
  raw SamplerDescriptor {..} = do
    label_ptr <- rawPtr samplerLabel
    n_addressModeU <- raw addressModeU
    n_addressModeV <- raw addressModeV
    n_addressModeW <- raw addressModeW
    n_magFilter <- raw magFilter
    n_minFilter <- raw minFilter
    n_mipmapFilter <- raw mipmapFilter
    n_compare <- raw samplerCompare
    pure $
      WGPUSamplerDescriptor.WGPUSamplerDescriptor
        { nextInChain = nullPtr,
          label = label_ptr,
          addressModeU = n_addressModeU,
          addressModeV = n_addressModeV,
          addressModeW = n_addressModeW,
          magFilter = n_magFilter,
          minFilter = n_minFilter,
          mipmapFilter = n_mipmapFilter,
          lodMinClamp = CFloat lodMinClamp,
          lodMaxClamp = CFloat lodMaxClamp,
          compare = n_compare,
          maxAnisotropy = maxAnisotropy
        }

-------------------------------------------------------------------------------

-- | Create a 'Sampler'.
createSampler ::
  MonadIO m =>
  -- | Device for which to create the sampler.
  Device ->
  -- | Description of the sampler to create.
  SamplerDescriptor ->
  -- | Action to create the sampler.
  m Sampler
createSampler device samplerDescriptor = liftIO . evalContT $ do
  let inst = deviceInst device
  samplerDescriptor_ptr <- rawPtr samplerDescriptor
  Sampler
    <$> RawFun.wgpuDeviceCreateSampler
      (wgpuHsInstance inst)
      (wgpuDevice device)
      samplerDescriptor_ptr
