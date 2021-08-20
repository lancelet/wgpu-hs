{-# LANGUAGE CPP #-}

module WGPU.Raw.Dynamic
  ( -- * Functions
    withWGPU,
  )
where

import WGPU.Raw.Generated.Fun (WGPUHsInstance, loadDynamicInstance)

#ifdef WGPUHS_UNIX
import System.Posix.DynamicLinker (withDL, dlsym)

-- | Load WGPU from a dynamic library and run a program using an instance.
withWGPU ::
  -- | Path to the wgpu-native dynamic library to load.
  FilePath ->
  -- | IO action using an instance of the WGPUHsInstance.
  (WGPUHsInstance -> IO a) ->
  -- | Completed IO action.
  IO a
withWGPU dynlibFile action = do
  withDL dynlibFile [] $ \dl -> loadDynamicInstance (dlsym dl) >>= action
#endif

#ifdef WGPUHS_WINDOWS
import Foreign (FunPtr, castPtrToFunPtr)
import System.Win32.DLL (loadLibrary, freeLibrary, getProcAddress)

-- | Load WGPU from a dynamic library and run a program using an instance.
withWGPU ::
  -- | Path to the wgpu-native dynamic library to load.
  FilePath ->
  -- | IO action using an instance of the WGPUHsInstance.
  (WGPUHsInstance -> IO a) ->
  -- | Completed IO action.
  IO a
withWGPU dynlibFile action = do
  -- TODO: safer bracketing
  hInstance <- loadLibrary dynlibFile
  let load :: String -> IO (FunPtr a)
      load = fmap castPtrToFunPtr . getProcAddress hInstance
  wgpuHsInstance <- loadDynamicInstance load
  result <- action wgpuHsInstance
  freeLibrary hInstance
  pure result
#endif
