{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : WGPU.Internal.Adapter
-- Description : Adapter (physical device).
module WGPU.Internal.Adapter
  ( -- * Types
    Adapter (..),

    -- * Functions
    requestAdapter,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Cont (evalContT)
import Foreign (nullPtr)
import Foreign.Ptr (Ptr)
import WGPU.Internal.Instance (Instance, wgpuHsInstance)
import WGPU.Internal.Memory
  ( ToRaw,
    freeHaskellFunPtr,
    newEmptyMVar,
    putMVar,
    raw,
    rawPtr,
    showWithPtr,
    takeMVar,
  )
import WGPU.Internal.Surface (Surface, surfaceInst)
import qualified WGPU.Raw.Generated.Fun as RawFun
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
