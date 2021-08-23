-- |
-- Module      : WGPU
-- Description : WebGPU Native
-- License     : BSD-3-Clause
-- Copyright   : Copyright (C) Jonathan Merritt 2021
-- Maintainer  : Jonathan Merritt <j.s.merritt@gmail.com>
-- Stability   : experimental
-- Portability : macOS, Linux, Windows
--
-- Layout of this module should be guided by the evolving
-- <https://www.w3.org/TR/webgpu/ WebGPU Specification>.
module WGPU
  ( -- ** Introduction
    -- $introduction

    -- * Initialization #initialization#
    -- $initialization
    Instance,
    withPlatformInstance,
    withInstance,

    -- * Surface #surface#
    -- $surface
    Surface,
    createGLFWSurface,

    -- * Adapter #adapter#
    -- $adapter
    Adapter,
    requestAdapter,

    -- * Device #device#
    -- $device
    Device,
    DeviceDescriptor (..),
    Limits (..),
    Features (..),
    requestDevice,

    -- * Textures and Views
    TextureView,
    TextureViewDimension (..),
    TextureFormat (..),
    TextureUsage (..),

    -- * Swapchain
    SwapChain,
    SwapChainDescriptor (..),
    PresentMode (..),
    getSwapChainPreferredFormat,
    createSwapChain,
    getSwapChainCurrentTextureView,
    swapChainPresent,

    -- * Samplers

    -- * Resource Binding
    BindGroupLayout,
    BindGroupLayoutDescriptor (..),
    BindGroupLayoutEntry (..),
    Binding (..),
    ShaderStage (..),
    BindingType (..),
    BufferBindingLayout (..),
    SamplerBindingLayout (..),
    TextureBindingLayout (..),
    StorageTextureBindingLayout (..),
    StorageTextureAccess (..),
    TextureSampleType (..),
    BufferBindingType (..),
    createBindGroupLayout,

    -- * Shader Modules
    ShaderModule,
    ShaderModuleDescriptor (..),
    ShaderSource (..),
    SPIRV (..),
    WGSL (..),
    ShaderEntryPoint (..),
    createShaderModule,
    createShaderModuleSPIRV,
    createShaderModuleWGSL,

    -- * Pipelines

    -- ** Compute

    -- ** Render
    PipelineLayout,
    RenderPipeline,
    PipelineLayoutDescriptor (..),
    RenderPipelineDescriptor (..),
    VertexFormat (..),
    VertexAttribute (..),
    InputStepMode (..),
    VertexBufferLayout (..),
    VertexState (..),
    PrimitiveTopology (..),
    IndexFormat (..),
    FrontFace (..),
    CullMode (..),
    PrimitiveState (..),
    StencilOperation (..),
    StencilState (..),
    DepthBiasState (..),
    DepthStencilState (..),
    MultisampleState (..),
    BlendFactor (..),
    BlendOperation (..),
    BlendComponent (..),
    BlendState (..),
    ColorWriteMask (..),
    ColorTargetState (..),
    FragmentState (..),
    createPipelineLayout,
    createRenderPipeline,
    colorWriteMaskAll,

    -- * Command Buffers
    CommandBuffer,

    -- * Command Encoding
    CommandEncoder,
    RenderPassEncoder,
    Color (..),
    LoadOp (..),
    StoreOp (..),
    Operations (..),
    RenderPassColorAttachment (..),
    RenderPassDepthStencilAttachment (..),
    RenderPassDescriptor (..),
    Range (..),
    createCommandEncoder,
    commandEncoderFinish,
    beginRenderPass,
    renderPassSetPipeline,
    renderPassDraw,
    endRenderPass,

    -- * Queue
    Queue,
    getQueue,
    queueSubmit,

    -- * Version
    Version (..),
    getVersion,
    versionToText,

    -- * Logging
    LogLevel (..),
    LogCallback,
    setLogLevel,
    logStdout,
    logLevelToText,

    -- * Multipurpose
    CompareFunction (..),

    -- * Extras

    -- ** Strict Maybe
    SMaybe (..),
  )
where

import WGPU.Internal.Adapter
import WGPU.Internal.Binding
import WGPU.Internal.Color
import WGPU.Internal.CommandBuffer
import WGPU.Internal.CommandEncoder
import WGPU.Internal.Device
import WGPU.Internal.Instance
import WGPU.Internal.Multipurpose
import WGPU.Internal.Pipeline
import WGPU.Internal.Queue
import WGPU.Internal.RenderPass
import WGPU.Internal.SMaybe
import WGPU.Internal.Shader
import WGPU.Internal.Surface
import WGPU.Internal.SwapChain
import WGPU.Internal.Texture

-------------------------------------------------------------------------------

-- $introduction
--
-- === Introduction to WebGPU
--
-- WebGPU is a future web standard for graphics and compute, developed by the
-- W3C. It is currently (August 2021) an emerging technology, and not yet
-- stable. In addition to its <https://www.w3.org/TR/webgpu/ JavaScript API>,
-- there are also early attempts to create a native binding (ie. a C language
-- binding). Two implementations of the native binding are:
--
--   * <https://github.com/gfx-rs/wgpu-native wgpu-native>: a Mozilla-backed
--     WebGPU native implementation, written in Rust, used in the Firefox web
--     browser. This is the binding to which this Haskell library is currently
--     tied.
--
--   * <https://dawn.googlesource.com/dawn dawn>: a Google-backed WebGPU native
--     implementation, written in C++, used in the Chrome web browser. In the
--     future, we hope to support this backend too.
--
-- The native bindings to WebGPU have the potential to become a portable,
-- next-generation GPU API which is easy to use. Vulkan is also currently
-- available across platforms, but it is very low-level. In the opinion of the
-- author of this package, Vulkan is very difficult to use directly from
-- Haskell. It would benefit greatly from a shim layer which performs common
-- tasks and streamlines many operations. Not unexpectedly, that is exactly the
-- role that WebGPU native can play.
--
-- === Platform Support
--
-- Currently, macOS (Metal), Windows and Linux are supported.
--
-- === Dependence on GLFW-b
--
-- This package currently uses only
-- <https://hackage.haskell.org/package/GLFW-b GLFW-b>
-- for windowing and event processing.
--
-- === Structure of Bindings
--
-- The bindings to @wgpu-native@ are structured in three packages:
--
--   1. The @wgpu-raw-hs-codegen@ package is a code generator for the raw
--      bindings. It creates all the packages named @WGPU.Raw.Generated.*@
--      (without exception!).
--
--   2. The <https://hackage.haskell.org/package/wgpu-raw-hs wgpu-raw-hs>
--      package provides raw bindings to @wgpu-native@. They are "raw" in the
--      sense that they contain raw pointers and are not usable without manual
--      construction of the C structs that must be passed to the API.
--
--   3. The @wgpu-hs@ package (this one) provides high-level bindings. These
--      bindings are written manually. They are improvements on the raw
--      bindings in the following ways:
--
--       - There are no more raw @Ptr@ types.
--
--       - There are no callbacks.
--
--       - Several parts of the API are tweaked slightly to more closely
--         resemble the Rust API.
--
--       - Names are de-duplicated.
--
-- === Native Library Handling
--
-- The native library for @wgpu-native@ is not required at compile-time for this
-- package. The library is loaded dynamically at runtime.

-------------------------------------------------------------------------------

-- $initialization
--
-- The first step in using these Haskell bindings is to obtain an 'Instance'.
-- This acts as a handle to the rest of the API. An 'Instance' is obtained at
-- runtime by loading a dynamic library containing the WGPU binding symbols.
-- Currently, only the C\/Rust library from
-- <https://github.com/gfx-rs/wgpu-native wgpu-native> is supported.
--
-- To load the dynamic library and obtain an instance, use the
-- 'withPlatformInstance' or 'withInstance' bracketing functions:
--
-- @
-- 'withPlatformInstance' (Just 'logStdout') $ \inst -> do
--   -- set the logging level (optional)
--   'setLogLevel' inst 'Warn'
--   -- run the rest of the program...
-- @
--
-- After creating an 'Instance', you may next want to
-- <WGPU.html#g:surface create a surface>.

-------------------------------------------------------------------------------

-- $surface
--
-- A 'Surface' is a handle to a platform-specific presentable surface, like a
-- window. Currently, only GLFW windows are supported for surface creation.
-- Once you have a GLFW window, you may create a 'Surface' for it using the
-- 'createGLFWSurface' function.
--
-- Once you have a 'Surface', the next step is usually to
-- <WGPU.html#g:adapter request an adapter> that is compatible with it.

-------------------------------------------------------------------------------

-- $adapter
--
-- An 'Adapter' is a handle to a physical device. For example, a physical
-- display adaptor (GPU) or a software renderer. Currently you obtain an adapter
-- using 'requestAdapter', which requests an adapter that is compatible with an
-- existing 'Surface'.
--
-- After obtaining an adapter, you will typically want to
-- <WGPU.html#g:device request a device>.

-------------------------------------------------------------------------------

-- $device
--
-- A 'Device' is an open connection to a graphics and/or compute device. A
-- 'Device' is created using the 'requestDevice' function.
--
-- (According to the WebGPU API documentation, a 'Device' may also be "lost".
-- However, it's not yet clear how that event will be signalled to the C API,
-- nor how to handle it.)
