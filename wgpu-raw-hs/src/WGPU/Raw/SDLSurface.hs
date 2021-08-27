{-# LANGUAGE CPP #-}

module WGPU.Raw.SDLSurface where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign (Ptr, alloca, castPtr, nullPtr, poke)
import qualified SDL
import Unsafe.Coerce (unsafeCoerce)
import qualified WGPU.Raw.Generated.Enum.WGPUSType as WGPUSType
import WGPU.Raw.Generated.Fun (WGPUHsInstance, wgpuInstanceCreateSurface)
import WGPU.Raw.Generated.Struct.WGPUChainedStruct
import WGPU.Raw.Generated.Struct.WGPUSurfaceDescriptor
import WGPU.Raw.Types (WGPUInstance (WGPUInstance), WGPUSurface)

#ifdef WGPUHS_TARGET_MACOS

import WGPU.Raw.Generated.Struct.WGPUSurfaceDescriptorFromMetalLayer

createSurface ::
  MonadIO m =>
  WGPUHsInstance ->
  SDL.Window ->
  m WGPUSurface
createSurface inst window = liftIO $ do
  nsWindow <- wgpuHsSDLToNSWindow (unsafeCoerce window)
  metalLayer <- wgpuHsMetalLayer nsWindow

  alloca $ \ptr_surfaceDescriptor -> do
    alloca $ \ptr_chainedStruct -> do
      alloca $ \ptr_surfaceDescriptorFromMetalLayer -> do

        let surfaceDescriptorFromMetalLayer =
              WGPUSurfaceDescriptorFromMetalLayer
              { chain =
                  WGPUChainedStruct
                  { next = nullPtr,
                    sType = WGPUSType.SurfaceDescriptorFromMetalLayer
                  },
                layer = metalLayer
              }
        poke ptr_surfaceDescriptorFromMetalLayer surfaceDescriptorFromMetalLayer

        let chainedStruct =
              WGPUChainedStruct
              { next = castPtr ptr_surfaceDescriptorFromMetalLayer,
                sType = WGPUSType.SurfaceDescriptorFromMetalLayer
              }
        poke ptr_chainedStruct chainedStruct

        let surfaceDescriptor =
              WGPUSurfaceDescriptor
              { nextInChain = ptr_chainedStruct,
                label = nullPtr
              }
        poke ptr_surfaceDescriptor surfaceDescriptor

        wgpuInstanceCreateSurface
          inst
          (WGPUInstance nullPtr)
          ptr_surfaceDescriptor

wgpuHsSDLToNSWindow :: MonadIO m => Ptr () -> m (Ptr ())
wgpuHsSDLToNSWindow = liftIO . wgpu_sdl_to_ns_window_IO

foreign import ccall "wgpuhs_sdl_to_ns_window"
  wgpu_sdl_to_ns_window_IO ::
    Ptr () ->
    IO (Ptr ())

wgpuHsMetalLayer :: MonadIO m => Ptr () -> m (Ptr ())
wgpuHsMetalLayer = liftIO . wgpuhs_metal_layer_IO

foreign import ccall "wgpuhs_metal_layer"
  wgpuhs_metal_layer_IO ::
    Ptr () ->
    IO (Ptr ())

#endif

#ifdef WGPUHS_TARGET_LINUX

import Data.Word (Word32)
import WGPU.Raw.Generated.Struct.WGPUSurfaceDescriptorFromXlib

createSurface ::
  MonadIO m =>
  WGPUHsInstance ->
  SDL.Window ->
  m WGPUSurface
createSurface inst sdlWin = liftIO $ do
  x11Display <- wgpuHsSDLToX11Display (unsafeCoerce sdlWin)
  x11Window <- wgpuHsSDLToX11Window (unsafeCoerce sdlWin)

  alloca $ \ptr_surfaceDescriptor -> do
    alloca $ \ptr_chainedStruct -> do
      alloca $ \ptr_surfaceDescriptorFromXlib -> do

        let surfaceDescriptorFromXlib =
              WGPUSurfaceDescriptorFromXlib
              { chain =
                  WGPUChainedStruct
                  { next = nullPtr,
                    sType = WGPUSType.SurfaceDescriptorFromXlib
                  },
                display = x11Display,
                window = fromIntegral x11Window
              }
        poke
          ptr_surfaceDescriptorFromXlib
          surfaceDescriptorFromXlib

        let chainedStruct =
             WGPUChainedStruct
               { next = castPtr ptr_surfaceDescriptorFromXlib,
                 sType = WGPUSType.SurfaceDescriptorFromXlib
               }
        poke ptr_chainedStruct chainedStruct

        let surfaceDescriptor =
              WGPUSurfaceDescriptor
                { nextInChain = ptr_chainedStruct,
                  label = nullPtr
                }
        poke ptr_surfaceDescriptor surfaceDescriptor

        wgpuInstanceCreateSurface
          inst
          (WGPUInstance nullPtr)
          ptr_surfaceDescriptor

wgpuHsSDLToX11Window :: MonadIO m => Ptr () -> m Word32
wgpuHsSDLToX11Window = liftIO . wgpu_sdl_to_x11_window_IO

foreign import ccall "wgpuhs_sdl_to_x11_window"
  wgpu_sdl_to_x11_window_IO ::
    Ptr () ->
    IO Word32

wgpuHsSDLToX11Display :: MonadIO m => Ptr () -> m (Ptr ())
wgpuHsSDLToX11Display = liftIO . wgpu_sdl_to_x11_display_IO

foreign import ccall "wgpuhs_sdl_to_x11_display"
  wgpu_sdl_to_x11_display_IO ::
    Ptr () ->
    IO (Ptr ())

#endif

#ifdef WGPUHS_TARGET_WINDOWS

import System.Win32.DLL (getModuleHandle)
import WGPU.Raw.Generated.Struct.WGPUSurfaceDescriptorFromWindowsHWND

createSurface ::
  MonadIO m =>
  WGPUHsInstance ->
  SDL.Window ->
  m WGPUSurface
createSurface inst window = liftIO $ do
  hWnd <- wgpuHsSDLToHWnd (unsafeCoerce window)
  hInstance <- getModuleHandle Nothing

  alloca $ \ptr_surfaceDescriptor -> do
    alloca $ \ptr_chainedStruct -> do
      alloca $ \ptr_surfaceDescriptorFromWindowsHWND -> do

        let surfaceDescriptorFromWindowsHWND =
              WGPUSurfaceDescriptorFromWindowsHWND
              { chain =
                  WGPUChainedStruct
                  { next = nullPtr,
                    sType = WGPUSType.SurfaceDescriptorFromWindowsHWND
                  },
                hinstance = hInstance,
                hwnd = hWnd
              }
        poke
          ptr_surfaceDescriptorFromWindowsHWND
          surfaceDescriptorFromWindowsHWND

        let chainedStruct =
              WGPUChainedStruct
              { next = castPtr ptr_surfaceDescriptorFromWindowsHWND,
                sType = WGPUSType.SurfaceDescriptorFromWindowsHWND
              }
        poke ptr_chainedStruct chainedStruct

        let surfaceDescriptor =
              WGPUSurfaceDescriptor
              { nextInChain = ptr_chainedStruct,
                label = nullPtr
              }
        poke ptr_surfaceDescriptor surfaceDescriptor

        wgpuInstanceCreateSurface
          inst
          (WGPUInstance nullPtr)
          ptr_surfaceDescriptor

wgpuHsSDLToHWnd :: MonadIO m => Ptr () -> m (Ptr ())
wgpuHsSDLToHWnd = liftIO . wgpuhs_sdl_to_hwnd_IO

foreign import ccall "wgpuhs_sdl_to_hwnd"
  wgpuhs_sdl_to_hwnd_IO ::
    Ptr () ->
    IO (Ptr ())

#endif
