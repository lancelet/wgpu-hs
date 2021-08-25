{-# LANGUAGE ForeignFunctionInterface #-}

module WGPU.Raw.Log
  ( -- * Functions
    connectLog,
    disconnectLog,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Foreign (nullFunPtr)
import WGPU.Raw.Generated.Fun (WGPUHsInstance, wgpuSetLogCallback)
import WGPU.Raw.Types (WGPULogCallback)

-- | Connect the supplied logging function for logging to stdout.
connectLog ::
  MonadIO m =>
  WGPUHsInstance ->
  m ()
connectLog inst = wgpuSetLogCallback inst wgpuhs_logging_callback

-- | Disconnnect the supplied logging function for logging to stdout.
disconnectLog ::
  MonadIO m =>
  WGPUHsInstance ->
  m ()
disconnectLog inst = wgpuSetLogCallback inst nullFunPtr

foreign import ccall "&wgpuhs_logging_callback"
  wgpuhs_logging_callback ::
    WGPULogCallback
