{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : WGPU.Internal.Instance
-- Description : Instance.
--
-- Instance of the WGPU API Haskell bindings.
module WGPU.Internal.Instance
  ( -- * Instance

    --
    -- $instance
    Instance (..),
    withPlatformInstance,
    withInstance,

    -- * Logging
    LogCallback,
    LogLevel (..),
    setLogLevel,
    logLevelToText,
    logStdout,

    -- * Version
    Version (..),
    getVersion,
    versionToText,
  )
where

import Data.Bits (shiftR, (.&.))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Word (Word32, Word8)
import Foreign (Ptr, freeHaskellFunPtr, nullFunPtr)
import Foreign.C (CChar, peekCString)
import qualified System.Info
import WGPU.Internal.Memory (ToRaw, raw)
import WGPU.Raw.Dynamic (withWGPU)
import WGPU.Raw.Generated.Enum.WGPULogLevel (WGPULogLevel (WGPULogLevel))
import qualified WGPU.Raw.Generated.Enum.WGPULogLevel as WGPULogLevel
import WGPU.Raw.Generated.Fun (WGPUHsInstance)
import qualified WGPU.Raw.Generated.Fun as RawFun
import WGPU.Raw.Types (WGPULogCallback)

-- $instance
--
-- The Haskell bindings to WGPU use a value of type 'Instance' as a handle to
-- the rest of the API. An 'Instance' value is obtained by loading a dynamic
-- library at runtime, using the 'withInstance' function. A typical invocation
-- might look like this:
--
-- @
-- 'withInstance' "libwgpu_native.dylib" (Just 'logStdOut') $ \inst -> do
--   -- set the logging level for the instance
--   'setLogLevel' inst 'Warn'
--   -- run the rest of the program ...
-- @
--
-- The dynamic library @libwgpu_native.dylib@ is obtained by compiling the
-- Rust project <https://github.com/gfx-rs/wgpu-native wgpu-native>. Care
-- should be take to compile a version of @libwgpu_native.dylib@ which is
-- compatible with the API in these bindings.

-------------------------------------------------------------------------------

-- | Instance of the WGPU API.
--
-- An instance is loaded from a dynamic library using the 'withInstance'
-- function.
newtype Instance = Instance {wgpuHsInstance :: WGPUHsInstance}

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
  -- | Optional logging callback. @'Just' 'logStdout'@ can be supplied here to
  --   print log messages to @stdout@ for debugging purposes.
  Maybe LogCallback ->
  -- | The Program. A function which takes an 'Instance' and returns an IO
  --   action that uses the instance.
  (Instance -> IO a) ->
  -- | IO action which loads the WGPU 'Instance', passes it to the program, and
  --   returns the result of running the program.
  IO a
withPlatformInstance = withInstance platformDylibName

-- | Load the WGPU API from a dynamic library and supply an 'Instance' to a
-- program.
withInstance ::
  -- | Name of the @wgpu-native@ dynamic library, or a complete path to it.
  FilePath ->
  -- | Optional logging callback. @'Just' 'logStdout'@ can be supplied here to
  --   print log messages to @stdout@ for debugging purposes.
  Maybe LogCallback ->
  -- | The Program. A function which takes an 'Instance' and returns an IO
  --   action that uses the instance.
  (Instance -> IO a) ->
  -- | IO action which loads the WGPU 'Instance', passes it to the program, and
  --   returns the result of running the program.
  IO a
withInstance dylibPath mLog program =
  withWGPU dylibPath $
    \winst -> do
      -- create the logging callback if necessary
      logCallback_c <- case mLog of
        Nothing -> pure nullFunPtr
        Just logFn -> mkLogCallback . toRawLogCallback $ logFn
      RawFun.wgpuSetLogCallback winst logCallback_c

      -- run the program
      result <- program (Instance winst)

      -- free the logging callback
      case mLog of
        Nothing -> pure ()
        Just _ -> do
          RawFun.wgpuSetLogCallback winst nullFunPtr
          freeHaskellFunPtr logCallback_c

      pure result

-- | Return the dynamic library name for a given platform.
--
-- This is the dynamic library name that should be passed to the 'withInstance'
-- function to load the dynamic library.
platformDylibName :: FilePath
platformDylibName =
  case System.Info.os of
    "darwin" -> "libwgpu_native.dylib"
    "mingw32" -> "wgpu_native.dll"
    "linux" -> "libwgpu_native.dylib"
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

-- | Logging callback function.
type LogCallback = LogLevel -> Text -> IO ()

-- | Set the current logging level for the instance.
setLogLevel :: Instance -> LogLevel -> IO ()
setLogLevel inst lvl =
  RawFun.wgpuSetLogLevel (wgpuHsInstance inst) (logLevelToWLogLevel lvl)

-- | Create a C callback from a Haskell logging function.
foreign import ccall "wrapper"
  mkLogCallback ::
    (WGPULogLevel -> Ptr CChar -> IO ()) ->
    IO WGPULogCallback

-- | Convert a logging callback function to the form required by the Raw API.
toRawLogCallback :: LogCallback -> (WGPULogLevel -> Ptr CChar -> IO ())
toRawLogCallback logFn wLogLevel cMsg = do
  msg <- Text.pack <$> peekCString cMsg
  logFn (wLogLevelToLogLevel wLogLevel) msg

-- | Convert a raw API logging level into a 'LogLevel'.
--
-- Any unknown log levels become an 'Error'.
wLogLevelToLogLevel :: WGPULogLevel -> LogLevel
wLogLevelToLogLevel wLvl =
  case wLvl of
    WGPULogLevel.Trace -> Trace
    WGPULogLevel.Debug -> Debug
    WGPULogLevel.Info -> Info
    WGPULogLevel.Warn -> Warn
    WGPULogLevel.Error -> Error
    _ -> Error

-- | Convert a 'LogLevel' value into the type required by the raw API.
logLevelToWLogLevel :: LogLevel -> WGPULogLevel
logLevelToWLogLevel lvl =
  case lvl of
    Trace -> WGPULogLevel.Trace
    Debug -> WGPULogLevel.Debug
    Info -> WGPULogLevel.Info
    Warn -> WGPULogLevel.Warn
    Error -> WGPULogLevel.Error

-- | Convert a 'LogLevel' to a text string.
logLevelToText :: LogLevel -> Text
logLevelToText lvl =
  case lvl of
    Trace -> "Trace"
    Debug -> "Debug"
    Info -> "Info"
    Warn -> "Warn"
    Error -> "Error"

-- | A logging function which prints to @stdout@.
--
-- This logging function can be supplied to 'withInstance' to print logging
-- messages to @stdout@ for debugging purposes.
logStdout :: LogLevel -> Text -> IO ()
logStdout lvl msg = TextIO.putStrLn $ "[" <> logLevelToText lvl <> "]: " <> msg

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
getVersion :: Instance -> IO Version
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
