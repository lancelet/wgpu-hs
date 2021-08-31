{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : WGPU.Classy
-- Description : Get parameters from readers.
--
-- WGPU commands commonly take parameters such as the 'Instance', 'Device', etc,
-- which are relatively unchanged across multiple commands. This module provides
-- a way to supply those parameters from a 'MonadReader'. Useful for the truly
-- lazy among us.
module WGPU.Classy
  ( -- * Constraints
    HasInstance,
    HasSurface,
    HasAdapter,
    HasDevice,
    HasSwapChain,
    HasCommandEncoder,
    HasRenderPassEncoder,
    HasQueue,

    -- * Classes

    -- * Lifted Functions

    -- ** Adapter
    requestAdapter,
    getAdapterProperties,

    -- ** Device
    requestDevice,

    -- ** Buffer
    createBuffer,
    createBufferInit,

    -- ** Texture
    createTexture,
    createView,

    -- ** Swapchain
    getSwapChainPreferredFormat,
    createSwapChain,
    getSwapChainCurrentTextureView,
    swapChainPresent,

    -- ** Samplers
    createSampler,

    -- ** Resource Binding
    createBindGroup,
    createBindGroupLayout,

    -- ** Shader Modules
    createShaderModule,
    createShaderModuleSPIRV,
    createShaderModuleWGSL,

    -- ** Pipelines

    -- *** Render
    createPipelineLayout,
    createRenderPipeline,

    -- ** Command Encoding
    createCommandEncoder,
    commandEncoderFinish,
    beginRenderPass,
    renderPassSetPipeline,
    renderPassDraw,
    renderPassSetBindGroup,
    renderPassSetIndexBuffer,
    renderPassSetVertexBuffer,
    renderPassDrawIndexed,
    endRenderPass,

    -- ** Queue
    getQueue,
    queueSubmit,
    queueSubmit',
    queueWriteTexture,
    queueWriteBuffer,

    -- ** Version
    getVersion,

    -- ** Logging
    connectLog,
    disconnectLog,
    setLogLevel,

    -- * Reader Contexts
    addEnv,

    -- * Building

    -- ** Command Encoding
    buildCommandBuffer,
    buildRenderPass,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, runReaderT)
import Data.Has (Has, getter)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word32, Word64)
import WGPU
  ( Adapter,
    AdapterProperties,
    BindGroup,
    BindGroupDescriptor,
    BindGroupLayout,
    BindGroupLayoutDescriptor,
    Buffer,
    BufferDescriptor,
    BufferUsage,
    CommandBuffer,
    CommandEncoder,
    Device,
    DeviceDescriptor,
    Extent3D,
    ImageCopyTexture,
    IndexFormat,
    Instance,
    LogLevel,
    PipelineLayout,
    PipelineLayoutDescriptor,
    Queue,
    Range,
    ReadableMemoryBuffer,
    RenderPassDescriptor,
    RenderPassEncoder,
    RenderPipeline,
    RenderPipelineDescriptor,
    SPIRV,
    Sampler,
    SamplerDescriptor,
    ShaderModule,
    ShaderModuleDescriptor,
    Surface,
    SwapChain,
    SwapChainDescriptor,
    Texture,
    TextureDataLayout,
    TextureDescriptor,
    TextureFormat,
    TextureView,
    TextureViewDescriptor,
    Version,
    WGSL,
  )
import qualified WGPU

-------------------------------------------------------------------------------
-- Constraint Synonyms

type RIO r m = (MonadIO m, MonadReader r m)

type HasInstance r m = (RIO r m, Has Instance r)

type HasSurface r m = (RIO r m, Has Surface r)

type HasAdapter r m = (RIO r m, Has Adapter r)

type HasDevice r m = (RIO r m, Has Device r)

type HasTexture r m = (RIO r m, Has Texture r)

type HasSwapChain r m = (RIO r m, Has SwapChain r)

type HasCommandEncoder r m = (RIO r m, Has CommandEncoder r)

type HasRenderPassEncoder r m = (RIO r m, Has RenderPassEncoder r)

type HasQueue r m = (RIO r m, Has Queue r)

access :: (Has q r, MonadReader r m) => (q -> m a) -> m a
access action = asks getter >>= action
{-# INLINEABLE access #-}

access2 :: (Has q r, MonadReader r m) => (q -> b -> m a) -> b -> m a
access2 action y = asks getter >>= \x -> action x y
{-# INLINEABLE access2 #-}

access3 :: (Has q r, MonadReader r m) => (q -> b -> c -> m a) -> b -> c -> m a
access3 action y z = asks getter >>= \x -> action x y z
{-# INLINEABLE access3 #-}

access4 ::
  (Has q r, MonadReader r m) =>
  (q -> b -> c -> d -> m a) ->
  b ->
  c ->
  d ->
  m a
access4 action x y z = asks getter >>= \w -> action w x y z
{-# INLINEABLE access4 #-}

access5 ::
  (Has q r, MonadReader r m) =>
  (q -> b -> c -> d -> e -> m a) ->
  b ->
  c ->
  d ->
  e ->
  m a
access5 action w x y z = asks getter >>= \v -> action v w x y z
{-# INLINEABLE access5 #-}

-------------------------------------------------------------------------------
-- Adapter

requestAdapter :: HasSurface r m => m (Maybe Adapter)
requestAdapter = access WGPU.requestAdapter
{-# INLINEABLE requestAdapter #-}

getAdapterProperties :: HasAdapter r m => m AdapterProperties
getAdapterProperties = access WGPU.getAdapterProperties
{-# INLINEABLE getAdapterProperties #-}

-------------------------------------------------------------------------------
-- Device

requestDevice :: HasAdapter r m => DeviceDescriptor -> m (Maybe Device)
requestDevice = access2 WGPU.requestDevice
{-# INLINEABLE requestDevice #-}

-------------------------------------------------------------------------------
-- Buffer

createBuffer :: HasDevice r m => BufferDescriptor -> m Buffer
createBuffer = access2 WGPU.createBuffer
{-# INLINEABLE createBuffer #-}

createBufferInit ::
  (HasDevice r m, ReadableMemoryBuffer a) =>
  Text ->
  BufferUsage ->
  a ->
  m Buffer
createBufferInit = access4 WGPU.createBufferInit
{-# INLINEABLE createBufferInit #-}

-------------------------------------------------------------------------------
-- Texture

createTexture :: HasDevice r m => TextureDescriptor -> m Texture
createTexture = access2 WGPU.createTexture
{-# INLINEABLE createTexture #-}

createView :: HasTexture r m => TextureViewDescriptor -> m TextureView
createView = access2 WGPU.createView
{-# INLINEABLE createView #-}

-------------------------------------------------------------------------------
-- Swapchain

getSwapChainPreferredFormat ::
  (HasSurface r m, HasAdapter r m) =>
  m TextureFormat
getSwapChainPreferredFormat =
  access . access2 $ WGPU.getSwapChainPreferredFormat
{-# INLINEABLE getSwapChainPreferredFormat #-}

createSwapChain ::
  (HasDevice r m, HasSurface r m) =>
  SwapChainDescriptor ->
  m SwapChain
createSwapChain = access2 . access3 $ WGPU.createSwapChain
{-# INLINEABLE createSwapChain #-}

getSwapChainCurrentTextureView :: HasSwapChain r m => m TextureView
getSwapChainCurrentTextureView = access WGPU.getSwapChainCurrentTextureView
{-# INLINEABLE getSwapChainCurrentTextureView #-}

swapChainPresent :: HasSwapChain r m => m ()
swapChainPresent = access WGPU.swapChainPresent
{-# INLINEABLE swapChainPresent #-}

-------------------------------------------------------------------------------
-- Samplers

createSampler :: (HasDevice r m) => SamplerDescriptor -> m Sampler
createSampler = access2 WGPU.createSampler
{-# INLINEABLE createSampler #-}

-------------------------------------------------------------------------------
-- Resource Binding

createBindGroup :: HasDevice r m => BindGroupDescriptor -> m BindGroup
createBindGroup = access2 WGPU.createBindGroup
{-# INLINEABLE createBindGroup #-}

createBindGroupLayout ::
  HasDevice r m =>
  BindGroupLayoutDescriptor ->
  m BindGroupLayout
createBindGroupLayout = access2 WGPU.createBindGroupLayout
{-# INLINEABLE createBindGroupLayout #-}

-------------------------------------------------------------------------------
-- Shader Modules

createShaderModule :: HasDevice r m => ShaderModuleDescriptor -> m ShaderModule
createShaderModule = access2 WGPU.createShaderModule
{-# INLINEABLE createShaderModule #-}

createShaderModuleSPIRV :: HasDevice r m => Text -> SPIRV -> m ShaderModule
createShaderModuleSPIRV = access3 WGPU.createShaderModuleSPIRV
{-# INLINEABLE createShaderModuleSPIRV #-}

createShaderModuleWGSL :: HasDevice r m => Text -> WGSL -> m ShaderModule
createShaderModuleWGSL = access3 WGPU.createShaderModuleWGSL
{-# INLINEABLE createShaderModuleWGSL #-}

-------------------------------------------------------------------------------
-- Render Pipelines

createPipelineLayout ::
  HasDevice r m =>
  PipelineLayoutDescriptor ->
  m PipelineLayout
createPipelineLayout = access2 WGPU.createPipelineLayout
{-# INLINEABLE createPipelineLayout #-}

createRenderPipeline ::
  HasDevice r m =>
  RenderPipelineDescriptor ->
  m RenderPipeline
createRenderPipeline = access2 WGPU.createRenderPipeline
{-# INLINEABLE createRenderPipeline #-}

-------------------------------------------------------------------------------
-- Command Encoding (Lifted)

createCommandEncoder :: HasDevice r m => Text -> m CommandEncoder
createCommandEncoder = access2 WGPU.createCommandEncoder
{-# INLINEABLE createCommandEncoder #-}

commandEncoderFinish :: HasCommandEncoder r m => Text -> m CommandBuffer
commandEncoderFinish = access2 WGPU.commandEncoderFinish
{-# INLINEABLE commandEncoderFinish #-}

beginRenderPass ::
  HasCommandEncoder r m =>
  RenderPassDescriptor ->
  m RenderPassEncoder
beginRenderPass = access2 WGPU.beginRenderPass
{-# INLINEABLE beginRenderPass #-}

renderPassSetPipeline :: HasRenderPassEncoder r m => RenderPipeline -> m ()
renderPassSetPipeline = access2 WGPU.renderPassSetPipeline
{-# INLINEABLE renderPassSetPipeline #-}

renderPassDraw ::
  HasRenderPassEncoder r m =>
  Range Word32 ->
  Range Word32 ->
  m ()
renderPassDraw = access3 WGPU.renderPassDraw
{-# INLINEABLE renderPassDraw #-}

renderPassSetBindGroup ::
  HasRenderPassEncoder r m =>
  Word32 ->
  BindGroup ->
  Vector Word32 ->
  m ()
renderPassSetBindGroup = access4 WGPU.renderPassSetBindGroup
{-# INLINEABLE renderPassSetBindGroup #-}

renderPassSetIndexBuffer ::
  HasRenderPassEncoder r m =>
  Buffer ->
  IndexFormat ->
  Word64 ->
  Word64 ->
  m ()
renderPassSetIndexBuffer = access5 WGPU.renderPassSetIndexBuffer
{-# INLINEABLE renderPassSetIndexBuffer #-}

renderPassSetVertexBuffer ::
  HasRenderPassEncoder r m =>
  Word32 ->
  Buffer ->
  Word64 ->
  Word64 ->
  m ()
renderPassSetVertexBuffer = access5 WGPU.renderPassSetVertexBuffer
{-# INLINEABLE renderPassSetVertexBuffer #-}

renderPassDrawIndexed ::
  HasRenderPassEncoder r m =>
  Range Word32 ->
  Int32 ->
  Range Word32 ->
  m ()
renderPassDrawIndexed = access4 WGPU.renderPassDrawIndexed
{-# INLINEABLE renderPassDrawIndexed #-}

endRenderPass :: HasRenderPassEncoder r m => m ()
endRenderPass = access WGPU.endRenderPass
{-# INLINEABLE endRenderPass #-}

-------------------------------------------------------------------------------
-- Queue

getQueue :: HasDevice r m => m Queue
getQueue = access WGPU.getQueue
{-# INLINEABLE getQueue #-}

queueSubmit :: HasQueue r m => Vector CommandBuffer -> m ()
queueSubmit = access2 WGPU.queueSubmit
{-# INLINEABLE queueSubmit #-}

-- | Fetch the queue from a device and submit command buffers to it.
queueSubmit' :: HasDevice r m => Vector CommandBuffer -> m ()
queueSubmit' = buildQueue . queueSubmit
{-# INLINEABLE queueSubmit' #-}

queueWriteTexture ::
  (HasQueue r m, ReadableMemoryBuffer a) =>
  ImageCopyTexture ->
  TextureDataLayout ->
  Extent3D ->
  a ->
  m ()
queueWriteTexture = access5 WGPU.queueWriteTexture
{-# INLINEABLE queueWriteTexture #-}

queueWriteBuffer ::
  (HasQueue r m, ReadableMemoryBuffer a) =>
  Buffer ->
  a ->
  m ()
queueWriteBuffer = access3 WGPU.queueWriteBuffer
{-# INLINEABLE queueWriteBuffer #-}

-------------------------------------------------------------------------------
-- Version

getVersion :: HasInstance r m => m Version
getVersion = access WGPU.getVersion

-------------------------------------------------------------------------------
-- Logging

connectLog :: HasInstance r m => m ()
connectLog = access WGPU.connectLog
{-# INLINEABLE connectLog #-}

disconnectLog :: HasInstance r m => m ()
disconnectLog = access WGPU.disconnectLog
{-# INLINEABLE disconnectLog #-}

setLogLevel :: HasInstance r m => LogLevel -> m ()
setLogLevel = access2 WGPU.setLogLevel
{-# INLINEABLE setLogLevel #-}

-------------------------------------------------------------------------------
-- Reader Contexts

-- | Add 'q' into the reader environment.
addEnv :: MonadReader r m => q -> ReaderT (q, r) m a -> m a
addEnv x action = ask >>= \env -> runReaderT action (x, env)
{-# INLINEABLE addEnv #-}

-------------------------------------------------------------------------------
-- Building

-- | Build a 'CommandBuffer' by running actions in an environment that has
-- access to a 'CommandEncoder'.
buildCommandBuffer ::
  forall r m.
  HasDevice r m =>
  -- | Debugging label for the command encoder.
  Text ->
  -- | Debugging label for the command buffer.
  Text ->
  -- | Action to configure the 'CommandEncoder'.
  ReaderT (CommandEncoder, r) m () ->
  -- | Completed 'CommandBuffer'.
  m CommandBuffer
buildCommandBuffer commandEncoderLabel commandBufferLabel build = do
  commandEncoder <- createCommandEncoder commandEncoderLabel
  addEnv commandEncoder (build >> commandEncoderFinish commandBufferLabel)
{-# INLINEABLE buildCommandBuffer #-}

-- | Build a render pass by running actions in an environment that has access
-- to a 'RenderPassEncoder'.
buildRenderPass ::
  forall r m.
  HasCommandEncoder r m =>
  RenderPassDescriptor ->
  ReaderT (RenderPassEncoder, r) m () ->
  m ()
buildRenderPass renderPassDescriptor build = do
  renderPassEncoder <- beginRenderPass renderPassDescriptor
  addEnv renderPassEncoder (build >> endRenderPass)
{-# INLINEABLE buildRenderPass #-}

-- | Build a queue by running actions in an environment that has access to a
-- `Queue`.
buildQueue :: HasDevice r m => ReaderT (Queue, r) m () -> m ()
buildQueue action = getQueue >>= \queue -> addEnv queue action
{-# INLINEABLE buildQueue #-}
