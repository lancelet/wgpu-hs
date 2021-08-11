{-# LANGUAGE ForeignFunctionInterface #-}

module WGPU
  ( -- * Types
    Surface,

    -- * Functions

    -- ** Initialization
    createSurface,
    requestCompatibleAdapter,
  )
where

import Foreign (Ptr, nullPtr)
import qualified SDL (Window)
import Unsafe.Coerce (unsafeCoerce)

-------------------------------------------------------------------------------
-- Helper C functions from this package

foreign import ccall "wgpuhs_create_surface"
  wgpuhs_create_surface :: Ptr () -> IO (Ptr ())

foreign import ccall "wgpuhs_request_compatible_adapter"
  wgpuhs_request_compatible_adapter :: Ptr () -> IO (Ptr ())

-------------------------------------------------------------------------------
-- Types

newtype Adapter = Adapter (Ptr ())

newtype Surface = Surface (Ptr ())

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
