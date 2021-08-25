{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : WGPU.Internal.Instance
-- Description : Instance.
--
-- Instance of the WGPU API Haskell bindings.
module WGPU.Internal.Instance
  ( -- * Instance
    Instance,
    wgpuHsInstance,
    withPlatformInstance,
    withInstance,

    -- * Logging
    LogLevel (..),
    setLogLevel,
    connectLog,
    disconnectLog,

    -- * Version
    Version (..),
    getVersion,
    versionToText,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Bits (shiftR, (.&.))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word32, Word8)
import qualified System.Info
import WGPU.Internal.Memory (ToRaw, raw)
import WGPU.Raw.Dynamic (InstanceHandle, instanceHandleInstance, withWGPU)
import WGPU.Raw.Generated.Enum.WGPULogLevel (WGPULogLevel)
import qualified WGPU.Raw.Generated.Enum.WGPULogLevel as WGPULogLevel
import WGPU.Raw.Generated.Fun (WGPUHsInstance)
import qualified WGPU.Raw.Generated.Fun as RawFun
import qualified WGPU.Raw.Log as RawLog

-------------------------------------------------------------------------------

-- | Instance of the WGPU API.
--
-- An instance is loaded from a dynamic library using the 'withInstance'
-- function.
newtype Instance = Instance {instanceHandle :: InstanceHandle}

wgpuHsInstance :: Instance -> WGPUHsInstance
wgpuHsInstance = instanceHandleInstance . instanceHandle

instance Show Instance where show _ = "<Instance>"

instance ToRaw Instance WGPUHsInstance where
  raw = pure . wgpuHsInstance

-------------------------------------------------------------------------------

-- | Load the WGPU API from a dynamic library and supply an 'Instance' to a
-- program.
--
-- This is the same as 'withInstance', except that it uses a default,
-- per-platform name for the library, based on the value returned by
-- 'System.Info.os'.
withPlatformInstance ::
  MonadIO m =>
  -- | Bracketing function.
  -- This can (for example) be something like 'Control.Exception.Safe.bracket'.
  (m Instance -> (Instance -> m ()) -> r) ->
  -- | Usage or action component of the bracketing function.
  r
withPlatformInstance = withInstance platformDylibName

-- | Load the WGPU API from a dynamic library and supply an 'Instance' to a
-- program.
withInstance ::
  forall m r.
  MonadIO m =>
  -- | Name of the @wgpu-native@ dynamic library, or a complete path to it.
  FilePath ->
  -- | Bracketing function.
  -- This can (for example) be something like 'Control.Exception.Safe.bracket'.
  (m Instance -> (Instance -> m ()) -> r) ->
  -- | Usage or action component of the bracketing function.
  r
withInstance dylibPath bkt = withWGPU dylibPath bkt'
  where
    bkt' :: m InstanceHandle -> (InstanceHandle -> m ()) -> r
    bkt' create release =
      bkt
        (Instance <$> create)
        (release . instanceHandle)

-- | Return the dynamic library name for a given platform.
--
-- This is the dynamic library name that should be passed to the 'withInstance'
-- function to load the dynamic library.
platformDylibName :: FilePath
platformDylibName =
  case System.Info.os of
    "darwin" -> "libwgpu_native.dylib"
    "mingw32" -> "wgpu_native.dll"
    "linux" -> "libwgpu_native.so"
    other ->
      error $ "platformDylibName: unknown / unhandled platform: " <> other

-------------------------------------------------------------------------------

-- | Logging level.
data LogLevel
  = Trace
  | Debug
  | Info
  | Warn
  | Error
  deriving (Eq, Show)

-- | Set the current logging level for the instance.
setLogLevel :: MonadIO m => Instance -> LogLevel -> m ()
setLogLevel inst lvl =
  RawFun.wgpuSetLogLevel (wgpuHsInstance inst) (logLevelToWLogLevel lvl)

-- | Convert a 'LogLevel' value into the type required by the raw API.
logLevelToWLogLevel :: LogLevel -> WGPULogLevel
logLevelToWLogLevel lvl =
  case lvl of
    Trace -> WGPULogLevel.Trace
    Debug -> WGPULogLevel.Debug
    Info -> WGPULogLevel.Info
    Warn -> WGPULogLevel.Warn
    Error -> WGPULogLevel.Error

-- | Connect a stdout logger to the instance.
connectLog :: MonadIO m => Instance -> m ()
connectLog inst = RawLog.connectLog (wgpuHsInstance inst)

-- | Disconnect a stdout logger from the instance.
disconnectLog :: MonadIO m => Instance -> m ()
disconnectLog inst = RawLog.disconnectLog (wgpuHsInstance inst)

-------------------------------------------------------------------------------

-- | Version of WGPU native.
data Version = Version
  { major :: !Word8,
    minor :: !Word8,
    patch :: !Word8,
    subPatch :: !Word8
  }
  deriving (Eq, Show)

-- | Return the exact version of the WGPU native instance.
getVersion :: MonadIO m => Instance -> m Version
getVersion inst = w32ToVersion <$> RawFun.wgpuGetVersion (wgpuHsInstance inst)
  where
    w32ToVersion :: Word32 -> Version
    w32ToVersion w =
      let major = fromIntegral $ (w `shiftR` 24) .&. 0xFF
          minor = fromIntegral $ (w `shiftR` 16) .&. 0xFF
          patch = fromIntegral $ (w `shiftR` 8) .&. 0xFF
          subPatch = fromIntegral $ w .&. 0xFF
       in Version {..}

-- | Convert a 'Version' value to a text string.
--
-- >>> versionToText (Version 0 9 2 2)
-- "v0.9.2.2"
versionToText :: Version -> Text
versionToText ver =
  "" <> showt (major ver)
    <> "."
    <> showt (minor ver)
    <> "."
    <> showt (patch ver)
    <> "."
    <> showt (subPatch ver)

-- | Show a value as a 'Text' string.
showt :: Show a => a -> Text
showt = Text.pack . show
