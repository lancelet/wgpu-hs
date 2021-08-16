{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- |
module WGPU.Raw.GLFWSurface where

import Foreign (Ptr, alloca, castPtr, nullPtr, poke)
import qualified Graphics.UI.GLFW as GLFW
import qualified WGPU.Raw.Generated.Enum.WGPUSType as WGPUSType
import WGPU.Raw.Generated.Fun (WGPUHsInstance, wgpuInstanceCreateSurface)
import WGPU.Raw.Generated.Struct.WGPUChainedStruct
import WGPU.Raw.Generated.Struct.WGPUSurfaceDescriptor
import WGPU.Raw.Generated.Struct.WGPUSurfaceDescriptorFromMetalLayer
import WGPU.Raw.Types (WGPUInstance (WGPUInstance), WGPUSurface)

#ifdef WGPUHS_TARGET_MACOS

createSurface ::
  WGPUHsInstance ->
  GLFW.Window ->
  IO WGPUSurface
createSurface inst window = do
  nsWindow <- GLFW.getCocoaWindow window
  metalLayer <- wgpuhs_metal_layer nsWindow

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

foreign import ccall "wgpuhs_metal_layer"
  wgpuhs_metal_layer ::
    Ptr () ->
    IO (Ptr ())

#endif

#ifdef WGPU_HS_TARGET_LINUX

createSurface ::
  WGPUHsInstance ->
  GLFW.Window ->
  IO WGPUSurface
createSurface inst window = error "Linux: not yet implemented."

#endif
