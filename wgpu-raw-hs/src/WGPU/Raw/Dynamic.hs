{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WGPU.Raw.Dynamic
  ( -- * Types
    InstanceHandle (..),

    -- * Functions
    withWGPU,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import WGPU.Raw.Generated.Fun (WGPUHsInstance, loadDynamicInstance)

#ifdef WGPUHS_UNIX

import System.Posix.DynamicLinker (dlsym, dlopen, dlclose, DL)
import System.Posix.DynamicLinker.Prim (RTLDFlags(RTLD_NOW))

data InstanceHandle = InstanceHandle
  { instanceHandleDL :: !DL,
    instanceHandleInstance :: !WGPUHsInstance
  }

-- | Load WGPU from a dynamic library and run a program using an instance.
withWGPU ::
  forall m r.
  MonadIO m =>
  -- | Path to the wgpu-native dynamic library to load.
  FilePath ->
  -- | Bracketing function.
  -- This can (for example) be something like 'Control.Exception.Safe.bracket'.
  (m InstanceHandle -> (InstanceHandle -> m ()) -> r) ->
  -- | Usage or action component of the bracketing function.
  r
withWGPU dynlibFile bkt = do
  let
    create :: m InstanceHandle
    create = do
      dl <- liftIO $ dlopen dynlibFile [RTLD_NOW]
      inst <- liftIO $ loadDynamicInstance (dlsym dl)
      pure (InstanceHandle dl inst)

    release :: InstanceHandle -> m ()
    release = liftIO . dlclose . instanceHandleDL

  bkt create release
#endif

#ifdef WGPUHS_WINDOWS
import Foreign (FunPtr, castPtrToFunPtr)
import System.Win32.DLL (loadLibrary, freeLibrary, getProcAddress)
import System.Win32.Types (HINSTANCE)

data InstanceHandle = InstanceHandle
  { instanceHandleDL :: !HINSTANCE,
    instanceHandleInstance :: !WGPUHsInstance
  }

-- | Load WGPU from a dynamic library and run a program using an instance.
withWGPU ::
  forall m r.
  MonadIO m =>
  -- | Path to the wgpu-native dynamic library to load.
  FilePath ->
  -- | Bracketing function.
  -- This can (for example) be something like 'Control.Exception.Safe.bracket'.
  (m InstanceHandle -> (InstanceHandle -> m ()) -> r) ->
  -- | Usage or action component of the bracketing function.
  r
withWGPU dynlibFile bkt = do
  let
    create :: m InstanceHandle
    create = do
      hInstance <- liftIO $ loadLibrary dynlibFile
      let load :: String -> IO (FunPtr a)
          load = fmap castPtrToFunPtr . getProcAddress hInstance
      inst <- liftIO $ loadDynamicInstance load
      pure (InstanceHandle hInstance inst)

    release :: InstanceHandle -> m ()
    release = liftIO . freeLibrary . instanceHandleDL

  bkt create release
#endif
