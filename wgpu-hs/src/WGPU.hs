-- |
-- Module      : WGPU
-- Description : WebGPU Native
-- License     : BSD-3-Clause
-- Copyright   : Copyright (C) Jonathan Merritt 2021
-- Maintainer  : Jonathan Merritt <j.s.merritt@gmail.com>
-- Stability   : experimental
-- Portability : macOS
--
-- Layout of this module should be guided by the evolving
-- <https://www.w3.org/TR/webgpu/ WebGPU Specification>.
module WGPU
  ( -- ** Introduction
    -- $introduction

    -- * Initialization #initialization#
    -- $initialization
    Instance,
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
-- Currently, only macOS (with the Metal backend) is supported. The limitation
-- is not fundamental and only exists because, so far, only macOS surface
-- creation has been implemented. In the future, other backends should be added.
--
-- === Dependence on GLFW-b
--
-- This package currently uses only
-- <https://hackage.haskell.org/package/GLFW-b GLFW-b>
-- for windowing and event processing. Clearly, it is undesirable to be
-- tied to only a single library for this purpose, when options like
-- <https://hackage.haskell.org/package/sdl2 sdl2> are available and might be
-- preferred by many users.
--
-- GFLW is used because it is the windowing library used in the C examples from
-- @wgpu-native@ and it exposes an API to obtain a pointer to the underlying
-- windowing system's native window. In the future, other options will be
-- investigated as time permits.
--
-- === Structure of Bindings
--
-- The bindings to @wgpu-native@ are structured in three packages:
--
--   1. The @wgpu-raw-hs-codegen@ package is a code generator for the raw
--      bindings. It creates all the packages named @WGPU.Raw.Generated.*@
--      (without exception!), using a custom code generator based on
--      `langage-c`. This package is not in Hackage, since it is only used
--      offline.
--
--   2. The <https://hackage.haskell.org/package/wgpu-raw-hs wgpu-raw-hs>
--      package provides raw bindings to @wgpu-native@. These raw bindings are
--      mostly auto-generated, but have some manual curation of top-level types
--      and function aliases. They are "raw" in the sense that they contain raw
--      pointers and are not usable without manual management of memory to
--      construct all the C structs that must be passed to the API.
--
--   3. The @wgpu-hs@ package (this one) provides high-level bindings. These
--      bindings are written manually. They are improvements on the raw
--      bindings in the following ways:
--
--       - There are no more raw @Ptr@ types. Memory for structs passed to the
--         raw API is managed using a type class (@ToRaw@) that encapsulates a
--         mapping between high-level API types and raw types. The possibility
--         to allocate memory as part of this conversion (and later free it) is
--         achieved by embedding conversion to the raw types inside the @ContT@
--         continuation monad.
--
--       - There are no callbacks. Several WebGPU native calls use callbacks to
--         indicate completion rather than blocking. The author decided that, in
--         the Haskell context, blocking was probably preferable. So,
--         internally, these calls are converted into a blocking form by waiting
--         on @MVar@s that are set by the callbacks.
--
--       - Several parts of the API are tweaked slightly to more closely
--         resemble the Rust API. This is done in cases where, for example, a
--         parameter to the C API is unused except in one branch of a sum type.
--         When this can be done easily enough, it is preferred to using the
--         flattened "union" approach.
--
--       - Names are de-duplicated. Where possible, names are identical to the C
--         API (sometimes with prefixes removed). However, where name conflicts
--         exist, names are changed to somewhat-idiomatic Haskell variants.
--
-- === Native Library Handling
--
-- The native library for @wgpu-native@ is not required at compile-time for this
-- package. Indeed, other packages containing executables that depend on this
-- one can be compiled without the native library! Instead, the library is
-- loaded dynamically and its symbols bound at runtime. This has the benefit
-- that the Haskell tooling need not be concerned with handling a Rust library
-- (yay!), but it is a point of common failure at runtime. To achieve this
-- independence, the header files for @wgpu-native@ are packaged inside
-- @wgpu-raw-hs@. Of course, care should be taken to ensure that a
-- fully-compatible version of the library is used at runtime.

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
-- 'withInstance' bracketing function:
--
-- @
-- 'withInstance' "libwgpu_native.dylib" (Just 'logStdout') $ \inst -> do
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
