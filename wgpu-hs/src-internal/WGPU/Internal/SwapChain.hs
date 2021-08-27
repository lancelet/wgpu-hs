{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

-- |
-- Module      : WGPU.Internal.SwapChain
-- Description : Swap chain.
module WGPU.Internal.SwapChain
  ( -- * Types
    SwapChain,
    SwapChainDescriptor (..),
    PresentMode (..),

    -- * Functions
    getSwapChainPreferredFormat,
    createSwapChain,
    getSwapChainCurrentTextureView,
    swapChainPresent,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Word (Word32)
import Foreign (Ptr, nullPtr)
import WGPU.Internal.Adapter (Adapter, wgpuAdapter)
import WGPU.Internal.Device (Device, deviceInst, wgpuDevice)
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
  )
import WGPU.Internal.Surface (Surface, surfaceInst, wgpuSurface)
import WGPU.Internal.Texture (TextureFormat, TextureUsage, TextureView (TextureView), textureFormatFromRaw)
import WGPU.Raw.Generated.Enum.WGPUPresentMode (WGPUPresentMode)
import qualified WGPU.Raw.Generated.Enum.WGPUPresentMode as WGPUPresentMode
import WGPU.Raw.Generated.Enum.WGPUTextureFormat (WGPUTextureFormat (WGPUTextureFormat))
import WGPU.Raw.Generated.Enum.WGPUTextureUsage (WGPUTextureUsage (WGPUTextureUsage))
import qualified WGPU.Raw.Generated.Fun as RawFun
import WGPU.Raw.Generated.Struct.WGPUSwapChainDescriptor (WGPUSwapChainDescriptor)
import qualified WGPU.Raw.Generated.Struct.WGPUSwapChainDescriptor as WGPUSwapChainDescriptor
import WGPU.Raw.Types (WGPUSurfaceGetPreferredFormatCallback, WGPUSwapChain (WGPUSwapChain))

-------------------------------------------------------------------------------

data SwapChain = SwapChain
  { swapChainInst :: !Instance,
    wgpuSwapChain :: !WGPUSwapChain
  }

instance Show SwapChain where
  show s =
    let SwapChain _ (WGPUSwapChain ptr) = s
     in showWithPtr "SwapChain" ptr

instance Eq SwapChain where
  (==) s1 s2 =
    let SwapChain _ (WGPUSwapChain s1_ptr) = s1
        SwapChain _ (WGPUSwapChain s2_ptr) = s2
     in s1_ptr == s2_ptr

instance ToRaw SwapChain WGPUSwapChain where
  raw = pure . wgpuSwapChain

-------------------------------------------------------------------------------

-- | Describes a swapchain.
data SwapChainDescriptor = SwapChainDescriptor
  { -- | Debugging label for the swap chain.
    swapChainLabel :: !Text,
    -- | The usage of the swap chain. The only supported usage is
    --   'TextureUsageRenderAttachment'.
    usage :: !TextureUsage,
    -- | Texture format of the swap chain. The only guaranteed formats are
    -- 'TextureFormatBgra8Unorm' and 'TextureFormatBgra8UnormSrgb'. To
    -- determine the preferred texture format for a surface, use the
    -- 'getSwapChainPreferredFormat' function.
    swapChainFormat :: !TextureFormat,
    -- | Width of the swap chain. Must be the same size as the surface.
    width :: !Word32,
    -- | Height of the swap chain. Must be the same size as the surface.
    height :: !Word32,
    -- | Presentation mode of the swap chain.
    presentMode :: !PresentMode
  }
  deriving (Eq, Show)

instance ToRaw SwapChainDescriptor WGPUSwapChainDescriptor where
  raw SwapChainDescriptor {..} = do
    label_ptr <- rawPtr swapChainLabel
    WGPUTextureUsage n_usage <- raw usage
    n_format <- raw swapChainFormat
    n_presentMode <- raw presentMode
    pure
      WGPUSwapChainDescriptor.WGPUSwapChainDescriptor
        { nextInChain = nullPtr,
          label = label_ptr,
          usage = n_usage,
          format = n_format,
          width = width,
          height = height,
          presentMode = n_presentMode
        }

-------------------------------------------------------------------------------

-- | Behaviour of the presentation engine based on frame rate.
data PresentMode
  = -- | The presentation engine does __not__ wait for a vertical blanking
    -- period and the request is presented immediately. This is a low-latency
    -- presentation mode, but visible tearing may be observed. Will fallback to
    -- @Fifo@ if unavailable on the selected platform and backend. Not optimal
    -- for mobile.
    PresentModeImmediate
  | -- | The presentation engine waits for the next vertical blanking period to
    -- update the current image, but frames may be submitted without delay. This
    -- is a low-latency presentation mode and visible tearing will not be
    -- observed. Will fallback to Fifo if unavailable on the selected platform
    -- and backend. Not optimal for mobile.
    PresentModeMailbox
  | -- | The presentation engine waits for the next vertical blanking period to
    -- update the current image. The framerate will be capped at the display
    -- refresh rate, corresponding to the VSync. Tearing cannot be observed.
    -- Optimal for mobile.
    PresentModeFifo
  deriving (Eq, Show)

instance ToRaw PresentMode WGPUPresentMode where
  raw pm =
    pure $
      case pm of
        PresentModeImmediate -> WGPUPresentMode.Immediate
        PresentModeMailbox -> WGPUPresentMode.Mailbox
        PresentModeFifo -> WGPUPresentMode.Fifo

-------------------------------------------------------------------------------

-- | Returns an optimal texture format to use for the swapchain with this
--   adapter and surface.
getSwapChainPreferredFormat ::
  MonadIO m =>
  -- | @Surface@ for which to obtain an optimal texture format.
  Surface ->
  -- | @Adapter@ for which to obtain an optimal texture format.
  Adapter ->
  -- | IO action which returns the optimal texture format.
  m TextureFormat
getSwapChainPreferredFormat surface adapter = do
  let inst = surfaceInst surface

  textureFormatMVar <- newEmptyMVar
  callback <-
    mkSurfaceGetPreferredFormatCallback (\tf _ -> putMVar textureFormatMVar tf)

  RawFun.wgpuSurfaceGetPreferredFormat
    (wgpuHsInstance inst)
    (wgpuSurface surface)
    (wgpuAdapter adapter)
    callback
    nullPtr

  textureFormat <- textureFormatFromRaw <$> takeMVar textureFormatMVar
  freeHaskellFunPtr callback
  pure textureFormat

mkSurfaceGetPreferredFormatCallback ::
  MonadIO m =>
  (WGPUTextureFormat -> Ptr () -> IO ()) ->
  m WGPUSurfaceGetPreferredFormatCallback
mkSurfaceGetPreferredFormatCallback =
  liftIO . mkSurfaceGetPreferredFormatCallbackIO

foreign import ccall "wrapper"
  mkSurfaceGetPreferredFormatCallbackIO ::
    (WGPUTextureFormat -> Ptr () -> IO ()) ->
    IO WGPUSurfaceGetPreferredFormatCallback

-------------------------------------------------------------------------------

-- | Createa a new 'SwapChain' which targets a 'Surface'.
--
-- To determine the preferred 'TextureFormat' for the 'Surface', use the
-- 'getSwapChainPreferredFormat' function.
createSwapChain ::
  MonadIO m =>
  -- | @Device@ for which the @SwapChain@ will be created.
  Device ->
  -- | @Surface@ for which the @SwapChain@ will be created.
  Surface ->
  -- | Description of the @SwapChain@ to be created.
  SwapChainDescriptor ->
  -- | IO action which creates the swap chain.
  m SwapChain
createSwapChain device surface scd = liftIO . evalContT $ do
  let inst = deviceInst device
  swapChainDescriptor_ptr <- rawPtr scd
  rawSwapChain <-
    RawFun.wgpuDeviceCreateSwapChain
      (wgpuHsInstance inst)
      (wgpuDevice device)
      (wgpuSurface surface)
      swapChainDescriptor_ptr
  pure (SwapChain inst rawSwapChain)

-------------------------------------------------------------------------------

-- | Get the 'TextureView' for the current swap chain frame.
getSwapChainCurrentTextureView ::
  MonadIO m =>
  -- | Swap chain from which to fetch the current texture view.
  SwapChain ->
  -- | IO action which returns the current swap chain texture view.
  m TextureView
getSwapChainCurrentTextureView swapChain = do
  let inst = swapChainInst swapChain
  TextureView
    <$> RawFun.wgpuSwapChainGetCurrentTextureView
      (wgpuHsInstance inst)
      (wgpuSwapChain swapChain)

-------------------------------------------------------------------------------

-- | Present the latest swap chain image.
swapChainPresent ::
  MonadIO m =>
  -- | Swap chain to present.
  SwapChain ->
  -- | IO action which presents the swap chain image.
  m ()
swapChainPresent swapChain = do
  let inst = swapChainInst swapChain
  RawFun.wgpuSwapChainPresent (wgpuHsInstance inst) (wgpuSwapChain swapChain)
