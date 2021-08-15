{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Create an SDL window and draw a triangle using WGPU.
--
-- Please note: This is more of a proof-of-concept test for the Raw API. The
-- intent is to create a much nicer API on top of the Raw API. So this example
-- isn't intended as a template for an actual application.
module Triangle where

import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (unless)
import Data.List (intersperse)
import Data.Word (Word32)
import Foreign (Ptr, alloca, castPtr, freeHaskellFunPtr, nullPtr, poke)
import Foreign.C (CChar, peekCString, withCString)
import Foreign.C.Types (CBool (CBool))
import qualified Graphics.UI.GLFW as GLFW
import System.Exit (exitFailure)
import WGPU.Raw.Dynamic (withWGPU)
import WGPU.Raw.GLFWSurface (createSurface)
import qualified WGPU.Raw.Generated.Enum.WGPUBlendFactor as WGPUBlendFactor
import qualified WGPU.Raw.Generated.Enum.WGPUBlendOperation as WGPUBlendOperation
import qualified WGPU.Raw.Generated.Enum.WGPUColorWriteMask as WGPUColorWriteMask
import qualified WGPU.Raw.Generated.Enum.WGPUCullMode as WGPUCullMode
import qualified WGPU.Raw.Generated.Enum.WGPUFrontFace as WGPUFrontFace
import qualified WGPU.Raw.Generated.Enum.WGPUIndexFormat as WGPUIndexFormat
import WGPU.Raw.Generated.Enum.WGPULogLevel (WGPULogLevel)
import qualified WGPU.Raw.Generated.Enum.WGPULogLevel as WGPULogLevel
import qualified WGPU.Raw.Generated.Enum.WGPUNativeSType as WGPUSType
import qualified WGPU.Raw.Generated.Enum.WGPUPresentMode as WGPUPresentMode
import qualified WGPU.Raw.Generated.Enum.WGPUPrimitiveTopology as WGPUPrimitiveTopology
import qualified WGPU.Raw.Generated.Enum.WGPUSType as WGPUSType
import qualified WGPU.Raw.Generated.Enum.WGPUStoreOp as WGPULoadOp
import qualified WGPU.Raw.Generated.Enum.WGPUStoreOp as WGPUStoreOp
import WGPU.Raw.Generated.Enum.WGPUTextureFormat (WGPUTextureFormat (WGPUTextureFormat))
import qualified WGPU.Raw.Generated.Enum.WGPUTextureUsage as WGPUTextureUsage
import WGPU.Raw.Generated.Fun
  ( WGPUHsInstance,
    wgpuAdapterRequestDevice,
    wgpuCommandEncoderBeginRenderPass,
    wgpuCommandEncoderFinish,
    wgpuDeviceCreateCommandEncoder,
    wgpuDeviceCreatePipelineLayout,
    wgpuDeviceCreateRenderPipeline,
    wgpuDeviceCreateShaderModule,
    wgpuDeviceCreateSwapChain,
    wgpuDeviceGetQueue,
    wgpuInstanceRequestAdapter,
    wgpuQueueSubmit,
    wgpuRenderPassEncoderDraw,
    wgpuRenderPassEncoderEndPass,
    wgpuRenderPassEncoderSetPipeline,
    wgpuSetLogCallback,
    wgpuSetLogLevel,
    wgpuSurfaceGetPreferredFormat,
    wgpuSwapChainGetCurrentTextureView,
    wgpuSwapChainPresent,
  )
import WGPU.Raw.Generated.Struct.WGPUBlendComponent
import WGPU.Raw.Generated.Struct.WGPUBlendState
import WGPU.Raw.Generated.Struct.WGPUChainedStruct
import WGPU.Raw.Generated.Struct.WGPUColor
import WGPU.Raw.Generated.Struct.WGPUColorTargetState
import WGPU.Raw.Generated.Struct.WGPUCommandBufferDescriptor
import WGPU.Raw.Generated.Struct.WGPUCommandEncoderDescriptor
import WGPU.Raw.Generated.Struct.WGPUDeviceDescriptor
import WGPU.Raw.Generated.Struct.WGPUDeviceExtras
import WGPU.Raw.Generated.Struct.WGPUFragmentState
import WGPU.Raw.Generated.Struct.WGPUMultisampleState
import WGPU.Raw.Generated.Struct.WGPUPipelineLayoutDescriptor
import WGPU.Raw.Generated.Struct.WGPUPrimitiveState
import WGPU.Raw.Generated.Struct.WGPURenderPassColorAttachment
import WGPU.Raw.Generated.Struct.WGPURenderPassDescriptor
import WGPU.Raw.Generated.Struct.WGPURenderPipelineDescriptor
import WGPU.Raw.Generated.Struct.WGPURequestAdapterOptions
import WGPU.Raw.Generated.Struct.WGPUShaderModuleDescriptor
import WGPU.Raw.Generated.Struct.WGPUShaderModuleWGSLDescriptor
import WGPU.Raw.Generated.Struct.WGPUSwapChainDescriptor
import WGPU.Raw.Generated.Struct.WGPUVertexState
import WGPU.Raw.Types
  ( WGPUAdapter (WGPUAdapter),
    WGPUCommandBuffer,
    WGPUCommandEncoder,
    WGPUDevice (WGPUDevice),
    WGPUInstance (WGPUInstance),
    WGPULogCallback,
    WGPUPipelineLayout,
    WGPUQuerySet (WGPUQuerySet),
    WGPUQueue,
    WGPURenderPassEncoder,
    WGPURenderPipeline,
    WGPURequestAdapterCallback,
    WGPURequestDeviceCallback,
    WGPUShaderModule,
    WGPUSurface,
    WGPUSurfaceGetPreferredFormatCallback,
    WGPUSwapChain,
    WGPUTextureView (WGPUTextureView),
  )

main :: IO ()
main = do
  putStrLn "Triangle example"

  let wwidth, wheight :: Int
      wwidth = 640
      wheight = 480

  initResult <- GLFW.init
  unless initResult $ do
    putStrLn "GLFW initialization failed"
    exitFailure

  GLFW.windowHint (GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI)
  window <- do
    mWindow <- GLFW.createWindow wwidth wheight "Triangle" Nothing Nothing
    case mWindow of
      Just w -> pure w
      Nothing -> do
        putStrLn "Could not create GLFW window"
        exitFailure

  withWGPU "libwgpu_native.dylib" $ \inst -> do
    withLog inst WGPULogLevel.Warn $ do
      surface <- createSurface inst window
      adapter <- getAdapterForSurface inst surface
      device <- getDevice inst adapter
      shader <- compileWGSL inst device "shader" shaderSrc
      pipelineLayout <- createPipelineLayout inst device
      swapChainFormat <- getSwapChainFormat inst surface adapter
      pipeline <-
        createRenderPipeline inst device pipelineLayout shader swapChainFormat
      swapChain <-
        createSwapChain inst device surface swapChainFormat wwidth wheight

      let loop = do
            -- render
            nextTexture <- wgpuSwapChainGetCurrentTextureView inst swapChain
            encoder <- createCommandEncoder inst device
            renderPass <- beginRenderPass inst encoder nextTexture
            wgpuRenderPassEncoderSetPipeline inst renderPass pipeline
            wgpuRenderPassEncoderDraw inst renderPass 3 1 0 0
            wgpuRenderPassEncoderEndPass inst renderPass
            queue <- wgpuDeviceGetQueue inst device
            cmdBuffer <- commandEncoderFinish inst encoder
            queueSubmit inst queue 1 cmdBuffer
            wgpuSwapChainPresent inst swapChain

            -- handle quit event
            GLFW.pollEvents
            shouldClose <- GLFW.windowShouldClose window
            unless shouldClose loop
      loop

  GLFW.destroyWindow window
  GLFW.terminate

shaderSrc :: String
shaderSrc =
  mconcat $
    intersperse
      "\n"
      [ "[[stage(vertex)]]",
        "fn vs_main([[builtin(vertex_index)]] in_vertex_index: u32) -> [[builtin(position)]] vec4<f32> {",
        "  let x = f32(i32(in_vertex_index) - 1);",
        "  let y = f32(i32(in_vertex_index & 1u) * 2 - 1);",
        "  return vec4<f32>(x, y, 0.0, 1.0);",
        "}",
        "",
        "[[stage(fragment)]]",
        "fn fs_main([[builtin(position)]] in: vec4<f32>) -> [[location(0)]] vec4<f32> {",
        "  return vec4<f32>(in.x/640.0, in.y/480.0, 1.0, 1.0);",
        "}"
      ]

-------------------------------------------------------------------------------

withLog :: WGPUHsInstance -> WGPULogLevel -> IO a -> IO a
withLog inst logLevel action = do
  let logCallback :: WGPULogLevel -> Ptr CChar -> IO ()
      logCallback lLvl cMsg = do
        let prefix =
              case lLvl of
                WGPULogLevel.Error -> "Error"
                WGPULogLevel.Warn -> "Warn"
                WGPULogLevel.Info -> "Info"
                WGPULogLevel.Debug -> "Debug"
                WGPULogLevel.Trace -> "Trace"
                _ -> "Unknown"
        msg <- peekCString cMsg
        putStrLn $ "[" <> prefix <> "] " <> msg

  logCallback_c :: WGPULogCallback <- mkLogCallback logCallback
  wgpuSetLogCallback inst logCallback_c
  wgpuSetLogLevel inst logLevel
  result <- action

  freeHaskellFunPtr logCallback_c

  pure result

foreign import ccall "wrapper"
  mkLogCallback ::
    (WGPULogLevel -> Ptr CChar -> IO ()) ->
    IO WGPULogCallback

-------------------------------------------------------------------------------

-- | Get a WGPU Adapter which matches the supplied surface.
getAdapterForSurface :: WGPUHsInstance -> WGPUSurface -> IO WGPUAdapter
getAdapterForSurface inst surface = do
  adapterMVar :: MVar WGPUAdapter <- newEmptyMVar

  let adapterCallback :: WGPUAdapter -> Ptr () -> IO ()
      adapterCallback adapter _ = putMVar adapterMVar adapter

  adapterCallback_c :: WGPURequestAdapterCallback <-
    mkAdapterCallback adapterCallback

  alloca $ \requestAdapterOptions_ptr -> do
    let requestAdapterOptions =
          WGPURequestAdapterOptions
            { nextInChain = nullPtr,
              compatibleSurface = surface
            }
    poke requestAdapterOptions_ptr requestAdapterOptions
    wgpuInstanceRequestAdapter
      inst
      (WGPUInstance nullPtr)
      requestAdapterOptions_ptr
      adapterCallback_c
      nullPtr

  adapter :: WGPUAdapter <- takeMVar adapterMVar
  freeHaskellFunPtr adapterCallback_c
  pure adapter

foreign import ccall "wrapper"
  mkAdapterCallback ::
    (WGPUAdapter -> Ptr () -> IO ()) -> IO WGPURequestAdapterCallback

-------------------------------------------------------------------------------

-- | Get a WGPU Device.
getDevice :: WGPUHsInstance -> WGPUAdapter -> IO WGPUDevice
getDevice inst adapter = do
  deviceMVar :: MVar WGPUDevice <- newEmptyMVar

  let deviceCallback :: WGPUDevice -> Ptr () -> IO ()
      deviceCallback device _ = putMVar deviceMVar device

  deviceCallback_c :: WGPURequestDeviceCallback <-
    mkDeviceCallback deviceCallback

  alloca $ \deviceDescriptor_ptr -> do
    alloca $ \chainedStruct_ptr -> do
      alloca $ \deviceExtras_ptr -> do
        let deviceExtras =
              WGPUDeviceExtras
                { chain =
                    WGPUChainedStruct
                      { next = nullPtr,
                        sType = WGPUSType.DeviceExtras
                      },
                  maxTextureDimension1D = 0,
                  maxTextureDimension2D = 0,
                  maxTextureDimension3D = 0,
                  maxTextureArrayLayers = 0,
                  maxBindGroups = 1,
                  maxDynamicStorageBuffersPerPipelineLayout = 0,
                  maxStorageBuffersPerShaderStage = 0,
                  maxStorageBufferBindingSize = 0,
                  nativeFeatures = 0,
                  label = nullPtr,
                  tracePath = nullPtr
                }
        poke deviceExtras_ptr deviceExtras

        let chainedStruct =
              WGPUChainedStruct
                { next = castPtr deviceExtras_ptr,
                  sType = WGPUSType.DeviceExtras
                }
        poke chainedStruct_ptr chainedStruct

        let deviceDescriptor =
              WGPUDeviceDescriptor
                { nextInChain = chainedStruct_ptr
                }
        poke deviceDescriptor_ptr deviceDescriptor

        wgpuAdapterRequestDevice
          inst
          adapter
          deviceDescriptor_ptr
          deviceCallback_c
          nullPtr

  device :: WGPUDevice <- takeMVar deviceMVar
  freeHaskellFunPtr deviceCallback_c
  pure device

foreign import ccall "wrapper"
  mkDeviceCallback ::
    (WGPUDevice -> Ptr () -> IO ()) -> IO WGPURequestDeviceCallback

-------------------------------------------------------------------------------

-- | Compile a shader.
compileWGSL ::
  -- | WGPUHs instance.
  WGPUHsInstance ->
  -- | WGPU Device.
  WGPUDevice ->
  -- | Name of the shader.
  String ->
  -- | Shader source.
  String ->
  -- | Compiled shader module.
  IO WGPUShaderModule
compileWGSL inst device name src = do
  withCString name $ \name_ptr -> do
    withCString src $ \src_ptr -> do
      alloca $ \shaderModuleDescriptor_ptr -> do
        alloca $ \chainedStruct_ptr -> do
          alloca $ \shaderModuleWGSLDescriptor_ptr -> do
            let shaderModuleWGSLDescriptor =
                  WGPUShaderModuleWGSLDescriptor
                    { chain =
                        WGPUChainedStruct
                          { next = nullPtr,
                            sType = WGPUSType.ShaderModuleWGSLDescriptor
                          },
                      source = src_ptr
                    }
            poke shaderModuleWGSLDescriptor_ptr shaderModuleWGSLDescriptor

            let chainedStruct =
                  WGPUChainedStruct
                    { next = castPtr shaderModuleWGSLDescriptor_ptr,
                      sType = WGPUSType.ShaderModuleWGSLDescriptor
                    }
            poke chainedStruct_ptr chainedStruct

            let shaderModuleDescriptor =
                  WGPUShaderModuleDescriptor
                    { nextInChain = chainedStruct_ptr,
                      label = name_ptr
                    }
            poke shaderModuleDescriptor_ptr shaderModuleDescriptor

            wgpuDeviceCreateShaderModule inst device shaderModuleDescriptor_ptr

-------------------------------------------------------------------------------

-- | Create pipeline layout.
createPipelineLayout :: WGPUHsInstance -> WGPUDevice -> IO WGPUPipelineLayout
createPipelineLayout inst device = do
  alloca $ \pipelineLayoutDescriptor_ptr -> do
    let pipelineLayoutDescriptor =
          WGPUPipelineLayoutDescriptor
            { nextInChain = nullPtr,
              label = nullPtr,
              bindGroupLayoutCount = 0,
              bindGroupLayouts = nullPtr
            }
    poke pipelineLayoutDescriptor_ptr pipelineLayoutDescriptor

    wgpuDeviceCreatePipelineLayout inst device pipelineLayoutDescriptor_ptr

-------------------------------------------------------------------------------

-- | Get preferred format for the swapchain
getSwapChainFormat ::
  WGPUHsInstance ->
  WGPUSurface ->
  WGPUAdapter ->
  IO WGPUTextureFormat
getSwapChainFormat inst surface adapter = do
  textureFormatMVar :: MVar WGPUTextureFormat <- newEmptyMVar

  let textureFormatCallback :: WGPUTextureFormat -> Ptr () -> IO ()
      textureFormatCallback tf _ = putMVar textureFormatMVar tf

  textureFormatCallback_c :: WGPUSurfaceGetPreferredFormatCallback <-
    mkSurfaceGetPreferredFormatCallback textureFormatCallback

  wgpuSurfaceGetPreferredFormat
    inst
    surface
    adapter
    textureFormatCallback_c
    nullPtr

  textureFormat :: WGPUTextureFormat <- takeMVar textureFormatMVar
  freeHaskellFunPtr textureFormatCallback_c
  pure textureFormat

foreign import ccall "wrapper"
  mkSurfaceGetPreferredFormatCallback ::
    (WGPUTextureFormat -> Ptr () -> IO ()) ->
    IO WGPUSurfaceGetPreferredFormatCallback

-------------------------------------------------------------------------------

createRenderPipeline ::
  WGPUHsInstance ->
  WGPUDevice ->
  WGPUPipelineLayout ->
  WGPUShaderModule ->
  WGPUTextureFormat ->
  IO WGPURenderPipeline
createRenderPipeline inst device pipelineLayout shader swapChainFormat = do
  withCString "vs_main" $ \vsEntryPoint_ptr -> do
    withCString "fs_main" $ \fsEntryPoint_ptr -> do
      alloca $ \blendState_ptr -> do
        alloca $ \colorTargetState_ptr -> do
          alloca $ \fragmentState_ptr -> do
            alloca $ \renderPipelineDescriptor_ptr -> do
              let blendState =
                    WGPUBlendState
                      { color =
                          WGPUBlendComponent
                            { srcFactor = WGPUBlendFactor.One,
                              dstFactor = WGPUBlendFactor.Zero,
                              operation = WGPUBlendOperation.Add
                            },
                        alpha =
                          WGPUBlendComponent
                            { srcFactor = WGPUBlendFactor.One,
                              dstFactor = WGPUBlendFactor.Zero,
                              operation = WGPUBlendOperation.Add
                            }
                      }
              poke blendState_ptr blendState

              let colorTargetState =
                    WGPUColorTargetState
                      { nextInChain = nullPtr,
                        format = swapChainFormat,
                        blend = blendState_ptr,
                        writeMask = WGPUColorWriteMask.All
                      }
              poke colorTargetState_ptr colorTargetState

              let fragmentState =
                    WGPUFragmentState
                      { nextInChain = nullPtr,
                        shaderModule = shader,
                        entryPoint = fsEntryPoint_ptr,
                        targetCount = 1,
                        targets = colorTargetState_ptr
                      }
              poke fragmentState_ptr fragmentState

              let renderPipelineDescriptor =
                    WGPURenderPipelineDescriptor
                      { nextInChain = nullPtr,
                        label = nullPtr,
                        layout = pipelineLayout,
                        vertex =
                          WGPUVertexState
                            { nextInChain = nullPtr,
                              shaderModule = shader,
                              entryPoint = vsEntryPoint_ptr,
                              bufferCount = 0,
                              buffers = nullPtr
                            },
                        primitive =
                          WGPUPrimitiveState
                            { nextInChain = nullPtr,
                              topology = WGPUPrimitiveTopology.TriangleList,
                              stripIndexFormat = WGPUIndexFormat.Undefined,
                              frontFace = WGPUFrontFace.CCW,
                              cullMode = WGPUCullMode.None
                            },
                        depthStencil = nullPtr,
                        multisample =
                          WGPUMultisampleState
                            { nextInChain = nullPtr,
                              count = 1,
                              mask = 0xFFFFFFFF,
                              alphaToCoverageEnabled = CBool 0
                            },
                        fragment = fragmentState_ptr
                      }
              poke renderPipelineDescriptor_ptr renderPipelineDescriptor

              wgpuDeviceCreateRenderPipeline
                inst
                device
                renderPipelineDescriptor_ptr

-------------------------------------------------------------------------------

createSwapChain ::
  WGPUHsInstance ->
  WGPUDevice ->
  WGPUSurface ->
  WGPUTextureFormat ->
  Int ->
  Int ->
  IO WGPUSwapChain
createSwapChain inst device surface swapChainFormat w h = do
  withCString "swapChain" $ \swapChainName_ptr ->
    alloca $ \swapChainDescriptor_ptr -> do
      let swapChainDescriptor =
            WGPUSwapChainDescriptor
              { nextInChain = nullPtr,
                label = swapChainName_ptr,
                usage = WGPUTextureUsage.RenderAttachment,
                format = swapChainFormat,
                width = fromIntegral w,
                height = fromIntegral h,
                presentMode = WGPUPresentMode.Fifo
              }
      poke swapChainDescriptor_ptr swapChainDescriptor
      wgpuDeviceCreateSwapChain inst device surface swapChainDescriptor_ptr

-------------------------------------------------------------------------------

createCommandEncoder ::
  WGPUHsInstance ->
  WGPUDevice ->
  IO WGPUCommandEncoder
createCommandEncoder inst device = do
  alloca $ \commandEncoderDescriptor_ptr -> do
    let commandEncoderDescriptor =
          WGPUCommandEncoderDescriptor
            { nextInChain = nullPtr,
              label = nullPtr
            }
    poke commandEncoderDescriptor_ptr commandEncoderDescriptor
    wgpuDeviceCreateCommandEncoder inst device commandEncoderDescriptor_ptr

-------------------------------------------------------------------------------

beginRenderPass ::
  WGPUHsInstance ->
  WGPUCommandEncoder ->
  WGPUTextureView ->
  IO WGPURenderPassEncoder
beginRenderPass inst cmdEncoder textureView = do
  alloca $ \renderPassColorAttachment_ptr -> do
    alloca $ \renderPassDescriptor_ptr -> do
      let renderPassColorAttachment =
            WGPURenderPassColorAttachment
              { view = textureView,
                resolveTarget = WGPUTextureView nullPtr,
                loadOp = WGPULoadOp.Clear,
                storeOp = WGPUStoreOp.Store,
                clearColor = WGPUColor 0 0 0 1
              }
      poke renderPassColorAttachment_ptr renderPassColorAttachment

      let renderPassDescriptor =
            WGPURenderPassDescriptor
              { nextInChain = nullPtr,
                label = nullPtr,
                colorAttachmentCount = 1,
                colorAttachments = renderPassColorAttachment_ptr,
                depthStencilAttachment = nullPtr,
                occlusionQuerySet = WGPUQuerySet nullPtr
              }
      poke renderPassDescriptor_ptr renderPassDescriptor

      wgpuCommandEncoderBeginRenderPass inst cmdEncoder renderPassDescriptor_ptr

-------------------------------------------------------------------------------

commandEncoderFinish ::
  WGPUHsInstance ->
  WGPUCommandEncoder ->
  IO WGPUCommandBuffer
commandEncoderFinish inst encoder = do
  alloca $ \commandBufferDescriptor_ptr -> do
    let commandBufferDescriptor =
          WGPUCommandBufferDescriptor
            { nextInChain = nullPtr,
              label = nullPtr
            }
    poke commandBufferDescriptor_ptr commandBufferDescriptor

    wgpuCommandEncoderFinish inst encoder commandBufferDescriptor_ptr

-------------------------------------------------------------------------------

queueSubmit ::
  WGPUHsInstance ->
  WGPUQueue ->
  Word32 ->
  WGPUCommandBuffer ->
  IO ()
queueSubmit inst queue n cmdBuffer = do
  alloca $ \commandBuffer_ptr -> do
    poke commandBuffer_ptr cmdBuffer

    wgpuQueueSubmit inst queue n commandBuffer_ptr
