{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module WGPU
  ( -- * Types
    Surface,

    -- * Functions

    -- ** Initialization
    createSurface,
    requestCompatibleAdapter,
    requestDevice,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word32)
import Foreign
  ( FunPtr,
    Ptr,
    Storable (alignment, peek, poke, sizeOf),
    nullPtr,
    peekByteOff,
    plusPtr, freeHaskellFunPtr, castPtr, alloca
  )
import Foreign.C.String (peekCString)
import qualified SDL (Window)
import Unsafe.Coerce (unsafeCoerce)
import WGPU.Types.DeviceDescriptor (DeviceDescriptor)
import qualified WGPU.CStruct as CStruct
import WGPU.Chain (PeekChain, PokeChain)
import Control.Concurrent.MVar (MVar, takeMVar, putMVar, newMVar, newEmptyMVar)

-------------------------------------------------------------------------------
-- Helper C functions from this package

foreign import ccall "wgpuhs_create_surface"
  wgpuhs_create_surface :: Ptr () -> IO (Ptr ())

foreign import ccall "wgpuhs_request_compatible_adapter"
  wgpuhs_request_compatible_adapter :: Ptr () -> IO (Ptr ())

-------------------------------------------------------------------------------
-- Callbacks

type WGPURequestDeviceCallback = Ptr () -> Ptr () -> IO ()

type WGPURequestDeviceCallbackPtr = FunPtr WGPURequestDeviceCallback

foreign import ccall "wrapper"
  createWGPURequestDeviceCallback ::
    WGPURequestDeviceCallback ->
    IO WGPURequestDeviceCallbackPtr

-------------------------------------------------------------------------------
-- wgpu C functions

foreign import ccall "wgpuAdapterRequestDevice"
  wgpuAdapterRequestDevice ::
    Ptr () ->
    Ptr () ->
    WGPURequestDeviceCallbackPtr ->
    Ptr () ->
    IO ()

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- Types from webgpu.h

newtype Adapter = Adapter (Ptr ())

newtype Surface = Surface (Ptr ())

newtype Device = Device (Ptr ())

-------------------------------------------------------------------------------
-- Operations

-- | Create a WGPU surface for an SDL window.
createSurface ::
  -- | SDL window in which to create the WGPU surface.
  SDL.Window ->
  -- | WGPU surface for drawing.
  IO Surface
createSurface window = do
  -- coerce the SDL window back to a raw pointer
  let windowPtr :: Ptr ()
      windowPtr = unsafeCoerce window

  -- create the surface
  surfacePtr <- wgpuhs_create_surface windowPtr

  -- report an error if we failed
  if surfacePtr == nullPtr
    then error "WGPUHS: ERROR: Could not create WGPU surface"
    else pure (Surface surfacePtr)

requestCompatibleAdapter :: Surface -> IO Adapter
requestCompatibleAdapter (Surface surfacePtr) = do
  adapterPtr <- wgpuhs_request_compatible_adapter surfacePtr
  if adapterPtr == nullPtr
    then error "WGPUHS: ERROR: Could not obtain compatible Adapter"
    else pure (Adapter adapterPtr)

requestDevice :: (PeekChain a, PokeChain a) => Adapter -> DeviceDescriptor a -> IO Device
requestDevice (Adapter adapterPtr) deviceDescriptor = do
  sentinel <- newEmptyMVar

  let hsCallback :: Ptr () -> Ptr () -> IO ()
      hsCallback device ptr = do
        poke (castPtr ptr) device
        putMVar sentinel ()

  cCallback <- createWGPURequestDeviceCallback hsCallback

  device <- CStruct.withCStruct deviceDescriptor $ \deviceDescriptorPtr ->
    alloca $ \userData -> do
      wgpuAdapterRequestDevice adapterPtr (castPtr deviceDescriptorPtr) cCallback userData
      takeMVar sentinel
      Device <$> peek (castPtr userData)

  freeHaskellFunPtr cCallback

  pure device
