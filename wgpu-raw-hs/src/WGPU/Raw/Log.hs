{-# LANGUAGE ForeignFunctionInterface #-}

module WGPU.Raw.Log
  ( -- * Functions
    connectLog,
    disconnectLog,
  )
where

import Foreign (nullFunPtr)
import WGPU.Raw.Generated.Fun (WGPUHsInstance, wgpuSetLogCallback)
import WGPU.Raw.Types (WGPULogCallback)

-- | Connect the supplied logging function for logging to stdout.
connectLog ::
  WGPUHsInstance ->
  IO ()
connectLog inst = wgpuSetLogCallback inst wgpuhs_logging_callback

-- | Disconnnect the supplied logging function for logging to stdout.
disconnectLog ::
  WGPUHsInstance ->
  IO ()
disconnectLog inst = wgpuSetLogCallback inst nullFunPtr

foreign import ccall "&wgpuhs_logging_callback"
  wgpuhs_logging_callback ::
    WGPULogCallback
