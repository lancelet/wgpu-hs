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

import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Cont (evalContT)
import Foreign (freeHaskellFunPtr, nullPtr)
import Foreign.Ptr (Ptr)
import WGPU.Internal.Instance (Instance, wgpuHsInstance)
import WGPU.Internal.Memory (ToRaw, raw, rawPtr, showWithPtr)
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
  -- | Existing surface for which to request an @Adapter@.
  Surface ->
  -- | The returned @Adapter@, if it could be retrieved.
  IO (Maybe Adapter)
requestAdapter surface = evalContT $ do
  let inst = surfaceInst surface

  adapterMVar :: MVar WGPUAdapter <- liftIO newEmptyMVar

  let adapterCallback :: WGPUAdapter -> Ptr () -> IO ()
      adapterCallback adapter _ = putMVar adapterMVar adapter
  adapterCallback_c <- liftIO $ mkAdapterCallback adapterCallback

  requestAdapterOptions_ptr <- rawPtr (RequestAdapterOptions surface)
  liftIO $
    RawFun.wgpuInstanceRequestAdapter
      (wgpuHsInstance inst)
      (WGPUInstance nullPtr)
      requestAdapterOptions_ptr
      adapterCallback_c
      nullPtr

  adapter <- liftIO $ takeMVar adapterMVar
  liftIO $ freeHaskellFunPtr adapterCallback_c

  pure $ case adapter of
    WGPUAdapter ptr | ptr == nullPtr -> Nothing
    WGPUAdapter _ -> Just (Adapter inst adapter)

foreign import ccall "wrapper"
  mkAdapterCallback ::
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
