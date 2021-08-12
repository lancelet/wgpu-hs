{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
module WGPU.Raw.Types where

import Data.Word (Word32)
import Foreign (FunPtr, Ptr, Storable)
import Foreign.C.Types (CChar)
import WGPU.Raw.Generated.Enum.WGPUBufferMapAsyncStatus
import WGPU.Raw.Generated.Enum.WGPUCreatePipelineAsyncStatus
import WGPU.Raw.Generated.Enum.WGPUErrorType
import WGPU.Raw.Generated.Enum.WGPULogLevel
import WGPU.Raw.Generated.Enum.WGPUQueueWorkDoneStatus
import WGPU.Raw.Generated.Enum.WGPUTextureFormat

-------------------------------------------------------------------------------
-- Opaque pointers

newtype WGPUAdapter = WGPUAdapter (Ptr ())
  deriving (Storable)

newtype WGPUBindGroup = WGPUBindGroup (Ptr ())
  deriving (Storable)

newtype WGPUBindGroupLayout = WGPUBindGroupLayout (Ptr ())
  deriving (Storable)

newtype WGPUBuffer = WGPUBuffer (Ptr ())
  deriving (Storable)

newtype WGPUCommandBuffer = WGPUCommandBuffer (Ptr ())
  deriving (Storable)

newtype WGPUCommandEncoder = WGPUCommandEncoder (Ptr ())
  deriving (Storable)

newtype WGPUComputePassEncoder = WGPUComputePassEncoder (Ptr ())
  deriving (Storable)

newtype WGPUComputePipeline = WGPUComputePipeline (Ptr ())
  deriving (Storable)

newtype WGPUDevice = WGPUDevice (Ptr ())
  deriving (Storable)

newtype WGPUInstance = WGPUInstance (Ptr ())
  deriving (Storable)

newtype WGPUPipelineLayout = WGPUPipelineLayout (Ptr ())
  deriving (Storable)

newtype WGPUQuerySet = WGPUQuerySet (Ptr ())
  deriving (Storable)

newtype WGPUQueue = WGPUQueue (Ptr ())
  deriving (Storable)

newtype WGPURenderBundle = WGPURenderBundle (Ptr ())
  deriving (Storable)

newtype WGPURenderBundleEncoder = WGPURenderBundleEncoder (Ptr ())
  deriving (Storable)

newtype WGPURenderPassEncoder = WGPURenderPassEncoder (Ptr ())
  deriving (Storable)

newtype WGPURenderPipeline = WGPURenderPipeline (Ptr ())
  deriving (Storable)

newtype WGPUSampler = WGPUSampler (Ptr ())
  deriving (Storable)

newtype WGPUShaderModule = WGPUShaderModule (Ptr ())
  deriving (Storable)

newtype WGPUSurface = WGPUSurface (Ptr ())
  deriving (Storable)

newtype WGPUSwapChain = WGPUSwapChain (Ptr ())
  deriving (Storable)

newtype WGPUTexture = WGPUTexture (Ptr ())
  deriving (Storable)

newtype WGPUTextureView = WGPUTextureView (Ptr ())
  deriving (Storable)

-------------------------------------------------------------------------------
-- Extra type aliases

type WGPUFlags = Word32

type WGPUBufferUsageFlags = WGPUFlags

type WGPUColorWriteMaskFlags = WGPUFlags

type WGPUMapModeFlags = WGPUFlags

type WGPUShaderStageFlags = WGPUFlags

type WGPUTextureUsageFlags = WGPUFlags

-------------------------------------------------------------------------------
-- Function pointers from webgpu.h

type WGPUProc = FunPtr (IO ())

type WGPURequestDeviceCallback =
  FunPtr
    ( WGPUDevice ->
      Ptr () ->
      IO ()
    )

type WGPUBufferMapCallback =
  FunPtr
    ( WGPUBufferMapAsyncStatus ->
      Ptr () ->
      IO ()
    )

type WGPUCreateComputePipelineAsyncCallback =
  FunPtr
    ( WGPUCreatePipelineAsyncStatus ->
      WGPUComputePipeline ->
      Ptr CChar ->
      Ptr () ->
      IO ()
    )

type WGPUCreateRenderPipelineAsyncCallback =
  FunPtr
    ( WGPUCreatePipelineAsyncStatus ->
      WGPURenderPipeline ->
      Ptr CChar ->
      Ptr () ->
      IO ()
    )

type WGPUDeviceLostCallback =
  FunPtr
    ( Ptr CChar ->
      Ptr () ->
      IO ()
    )

type WGPUErrorCallback =
  FunPtr
    ( WGPUErrorType ->
      Ptr CChar ->
      Ptr () ->
      IO ()
    )

type WGPURequestAdapterCallback =
  FunPtr
    ( WGPUAdapter ->
      Ptr () ->
      IO ()
    )

type WGPUQueueWorkDoneCallback =
  FunPtr
    ( WGPUQueueWorkDoneStatus ->
      Ptr () ->
      IO ()
    )

type WGPUSurfaceGetPreferredFormatCallback =
  FunPtr
    ( WGPUTextureFormat ->
      Ptr () ->
      IO ()
    )

-------------------------------------------------------------------------------
-- Function pointers from wgpu.h

type WGPULogCallback =
  FunPtr
    ( WGPULogLevel ->
      Ptr CChar ->
      IO ()
    )
