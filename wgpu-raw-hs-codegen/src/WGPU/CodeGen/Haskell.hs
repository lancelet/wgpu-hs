{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
module WGPU.CodeGen.Haskell
  ( -- * Types
    HaskellApi (HaskellApi, haskellApiEnums, haskellApiStructs, haskellApiFuns),
    HsEnumW32 (..),
    HsEnumW32Member (..),
    HsStruct (..),
    HsStructMember (..),
    HsFun (..),
    HsFunParam (..),

    -- * Functions
    haskellApi,
  )
where

import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word32)
import WGPU.CodeGen.Parse (CApi, CType)
import qualified WGPU.CodeGen.Parse as Parse

-------------------------------------------------------------------------------

data HaskellApi = HaskellApi
  { haskellApiEnums :: [HsEnumW32],
    haskellApiStructs :: [HsStruct],
    haskellApiFuns :: [HsFun]
  }
  deriving (Eq, Show)

haskellApi :: CApi -> HaskellApi
haskellApi cApi =
  let haskellApiEnums =
        hsEnumW32 cApi
          <$> [ -- from webgpu.h
                "WGPUAdapterType",
                "WGPUAddressMode",
                "WGPUBackendType",
                "WGPUBlendFactor",
                "WGPUBlendOperation",
                "WGPUBufferBindingType",
                "WGPUBufferMapAsyncStatus",
                "WGPUCompareFunction",
                "WGPUCreatePipelineAsyncStatus",
                "WGPUCullMode",
                "WGPUErrorFilter",
                "WGPUErrorType",
                "WGPUFilterMode",
                "WGPUFrontFace",
                "WGPUIndexFormat",
                "WGPUInputStepMode",
                "WGPULoadOp",
                "WGPUPipelineStatisticName",
                "WGPUPresentMode",
                "WGPUPrimitiveTopology",
                "WGPUQueryType",
                "WGPUQueueWorkDoneStatus",
                "WGPUSType",
                "WGPUSamplerBindingType",
                "WGPUStencilOperation",
                "WGPUStorageTextureAccess",
                "WGPUStoreOp",
                "WGPUTextureAspect",
                "WGPUTextureComponentType",
                "WGPUTextureDimension",
                "WGPUTextureFormat",
                "WGPUTextureSampleType",
                "WGPUTextureViewDimension",
                "WGPUVertexFormat",
                "WGPUBufferUsage",
                "WGPUColorWriteMask",
                "WGPUMapMode",
                "WGPUShaderStage",
                "WGPUTextureUsage",
                -- from wgpu.h
                "WGPUNativeSType",
                "WGPUNativeFeature",
                "WGPULogLevel"
              ]
      haskellApiStructs =
        hsStruct cApi
          <$> [ -- from webgpu.h
                "WGPUChainedStruct",
                "WGPUAdapterProperties",
                "WGPUBindGroupEntry",
                "WGPUBlendComponent",
                "WGPUBufferBindingLayout",
                "WGPUBufferDescriptor",
                "WGPUColor",
                "WGPUCommandBufferDescriptor",
                "WGPUCommandEncoderDescriptor",
                "WGPUComputePassDescriptor",
                "WGPUDeviceDescriptor",
                "WGPUExtent3D",
                "WGPUInstanceDescriptor",
                "WGPUMultisampleState",
                "WGPUOrigin3D",
                "WGPUPipelineLayoutDescriptor",
                "WGPUPrimitiveDepthClampingState",
                "WGPUPrimitiveState",
                "WGPUProgrammableStageDescriptor",
                "WGPUQuerySetDescriptor",
                "WGPURenderBundleDescriptor",
                "WGPURenderBundleEncoderDescriptor",
                "WGPURenderPassDepthStencilAttachment",
                "WGPURequestAdapterOptions",
                "WGPUSamplerBindingLayout",
                "WGPUSamplerDescriptor",
                "WGPUShaderModuleDescriptor",
                "WGPUShaderModuleSPIRVDescriptor",
                "WGPUShaderModuleWGSLDescriptor",
                "WGPUStencilFaceState",
                "WGPUStorageTextureBindingLayout",
                "WGPUSurfaceDescriptor",
                "WGPUSurfaceDescriptorFromCanvasHTMLSelector",
                "WGPUSurfaceDescriptorFromMetalLayer",
                "WGPUSurfaceDescriptorFromWindowsHWND",
                "WGPUSurfaceDescriptorFromXlib",
                "WGPUSwapChainDescriptor",
                "WGPUTextureBindingLayout",
                "WGPUTextureDataLayout",
                "WGPUTextureViewDescriptor",
                "WGPUVertexAttribute",
                "WGPUBindGroupDescriptor",
                "WGPUBindGroupLayoutEntry",
                "WGPUBlendState",
                "WGPUComputePipelineDescriptor",
                "WGPUDepthStencilState",
                "WGPUImageCopyBuffer",
                "WGPUImageCopyTexture",
                "WGPURenderPassColorAttachment",
                "WGPUTextureDescriptor",
                "WGPUVertexBufferLayout",
                "WGPUBindGroupLayoutDescriptor",
                "WGPUColorTargetState",
                "WGPURenderPassDescriptor",
                "WGPUVertexState",
                "WGPUFragmentState",
                "WGPURenderPipelineDescriptor",
                -- from wgpu.h
                "WGPUAdapterExtras",
                "WGPUDeviceExtras"
              ]
      haskellApiFuns =
        hsFun cApi
          <$> [ -- from webgpu.h
                "wgpuCreateInstance",
                "wgpuGetProcAddress",
                "wgpuAdapterGetProperties",
                "wgpuAdapterRequestDevice",
                "wgpuBufferDestroy",
                "wgpuBufferGetConstMappedRange",
                "wgpuBufferGetMappedRange",
                "wgpuBufferMapAsync",
                "wgpuBufferUnmap",
                "wgpuCommandEncoderBeginComputePass",
                "wgpuCommandEncoderBeginRenderPass",
                "wgpuCommandEncoderCopyBufferToBuffer",
                "wgpuCommandEncoderCopyBufferToTexture",
                "wgpuCommandEncoderCopyTextureToTexture",
                "wgpuCommandEncoderCopyTextureToBuffer",
                "wgpuCommandEncoderFinish",
                "wgpuCommandEncoderInsertDebugMarker",
                "wgpuCommandEncoderPopDebugGroup",
                "wgpuCommandEncoderPushDebugGroup",
                "wgpuCommandEncoderResolveQuerySet",
                "wgpuCommandEncoderWriteTimestamp",
                "wgpuComputePassEncoderBeginPipelineStatisticsQuery",
                "wgpuComputePassEncoderDispatch",
                "wgpuComputePassEncoderDispatchIndirect",
                "wgpuComputePassEncoderEndPass",
                "wgpuComputePassEncoderEndPipelineStatisticsQuery",
                "wgpuComputePassEncoderInsertDebugMarker",
                "wgpuComputePassEncoderPopDebugGroup",
                "wgpuComputePassEncoderPushDebugGroup",
                "wgpuComputePassEncoderSetBindGroup",
                "wgpuComputePassEncoderSetPipeline",
                "wgpuComputePassEncoderWriteTimestamp",
                "wgpuComputePipelineGetBindGroupLayout",
                "wgpuDeviceCreateBindGroup",
                "wgpuDeviceCreateBindGroupLayout",
                "wgpuDeviceCreateBuffer",
                "wgpuDeviceCreateCommandEncoder",
                "wgpuDeviceCreateComputePipeline",
                "wgpuDeviceCreateComputePipelineAsync",
                "wgpuDeviceCreatePipelineLayout",
                "wgpuDeviceCreateQuerySet",
                "wgpuDeviceCreateRenderBundleEncoder",
                "wgpuDeviceCreateRenderPipeline",
                "wgpuDeviceCreateRenderPipelineAsync",
                "wgpuDeviceCreateSampler",
                "wgpuDeviceCreateShaderModule",
                "wgpuDeviceCreateSwapChain",
                "wgpuDeviceCreateTexture",
                "wgpuDeviceGetQueue",
                "wgpuDevicePopErrorScope",
                "wgpuDevicePushErrorScope",
                "wgpuDeviceSetDeviceLostCallback",
                "wgpuDeviceSetUncapturedErrorCallback",
                "wgpuInstanceCreateSurface",
                "wgpuInstanceProcessEvents",
                "wgpuInstanceRequestAdapter",
                "wgpuQuerySetDestroy",
                "wgpuQueueOnSubmittedWorkDone",
                "wgpuQueueSubmit",
                "wgpuQueueWriteBuffer",
                "wgpuQueueWriteTexture",
                "wgpuRenderBundleEncoderDraw",
                "wgpuRenderBundleEncoderDrawIndexed",
                "wgpuRenderBundleEncoderDrawIndexedIndirect",
                "wgpuRenderBundleEncoderDrawIndirect",
                "wgpuRenderBundleEncoderFinish",
                "wgpuRenderBundleEncoderInsertDebugMarker",
                "wgpuRenderBundleEncoderPopDebugGroup",
                "wgpuRenderBundleEncoderPushDebugGroup",
                "wgpuRenderBundleEncoderSetBindGroup",
                "wgpuRenderBundleEncoderSetIndexBuffer",
                "wgpuRenderBundleEncoderSetPipeline",
                "wgpuRenderBundleEncoderSetVertexBuffer",
                "wgpuRenderPassEncoderBeginOcclusionQuery",
                "wgpuRenderPassEncoderBeginPipelineStatisticsQuery",
                "wgpuRenderPassEncoderDraw",
                "wgpuRenderPassEncoderDrawIndexed",
                "wgpuRenderPassEncoderDrawIndexedIndirect",
                "wgpuRenderPassEncoderDrawIndirect",
                "wgpuRenderPassEncoderEndOcclusionQuery",
                "wgpuRenderPassEncoderEndPass",
                "wgpuRenderPassEncoderEndPipelineStatisticsQuery",
                "wgpuRenderPassEncoderExecuteBundles",
                "wgpuRenderPassEncoderInsertDebugMarker",
                "wgpuRenderPassEncoderPopDebugGroup",
                "wgpuRenderPassEncoderPushDebugGroup",
                "wgpuRenderPassEncoderSetBindGroup",
                "wgpuRenderPassEncoderSetBlendConstant",
                "wgpuRenderPassEncoderSetIndexBuffer",
                "wgpuRenderPassEncoderSetPipeline",
                "wgpuRenderPassEncoderSetScissorRect",
                "wgpuRenderPassEncoderSetStencilReference",
                "wgpuRenderPassEncoderSetVertexBuffer",
                "wgpuRenderPassEncoderSetViewport",
                "wgpuRenderPassEncoderWriteTimestamp",
                "wgpuRenderPipelineGetBindGroupLayout",
                "wgpuSurfaceGetPreferredFormat",
                "wgpuSwapChainGetCurrentTextureView",
                "wgpuSwapChainPresent",
                "wgpuTextureCreateView",
                "wgpuTextureDestroy",
                -- from wgpu.h
                "wgpuDevicePoll",
                "wgpuSetLogCallback",
                "wgpuSetLogLevel",
                "wgpuGetVersion",
                "wgpuRenderPassEncoderSetPushConstants"
              ]
   in HaskellApi {..}

-------------------------------------------------------------------------------

-- | An enumeration.
data HsEnumW32 = HsEnumW32
  { hsEnumW32Name :: !Text,
    hsEnumW32Members :: [HsEnumW32Member]
  }
  deriving (Eq, Show)

-- | A member of a an enumeration.
data HsEnumW32Member = HsEnumW32Member
  { hsEnumW32MemberName :: !Text,
    hsEnumW32MemberValue :: !Word32
  }
  deriving (Eq, Show)

-- | Extract a 'Word32' enum from the C API and return it as an enum.
hsEnumW32 ::
  -- | The C API.
  CApi ->
  -- | Name of the enum to extract.
  Text ->
  -- | Enumeration.
  HsEnumW32
hsEnumW32 cApi name =
  let name' :: Text
      members' :: [Parse.CEnumW32Member]
      Parse.CEnumW32 name' members' = Parse.cApiEnums cApi Map.! name

      membersNoForce32 :: [Parse.CEnumW32Member]
      membersNoForce32 =
        filter
          ( \(Parse.CEnumW32Member ename _) ->
              not $ "Force32" `Text.isSuffixOf` ename
          )
          members'

      members :: [HsEnumW32Member]
      members = transformMember <$> membersNoForce32
        where
          transformMember :: Parse.CEnumW32Member -> HsEnumW32Member
          transformMember (Parse.CEnumW32Member k v) = HsEnumW32Member k v
   in HsEnumW32 name' members

-------------------------------------------------------------------------------

-- | A data type / struct.
data HsStruct = HsStruct
  { hsStructName :: !Text,
    hsStructMembers :: [HsStructMember]
  }
  deriving (Eq, Show)

-- | A member of a Haskell struct.
data HsStructMember = HsStructMember
  { hsStructMemberName :: !Text,
    hsStructMemberType :: !CType
  }
  deriving (Eq, Show)

-- | Extract a Struct from the C API.
hsStruct ::
  -- | The C API.
  CApi ->
  -- | Name of the struct to extract.
  Text ->
  -- | Haskell struct.
  HsStruct
hsStruct cApi name =
  let members' :: [Parse.CStructMember]
      Parse.CStruct _ members' = Parse.cApiStructs cApi Map.! name

      transformMember :: Parse.CStructMember -> HsStructMember
      transformMember (Parse.CStructMember n t) = HsStructMember n t

      members :: [HsStructMember]
      members = transformMember <$> members'
   in HsStruct name members

-------------------------------------------------------------------------------

-- | A function.
data HsFun = HsFun
  { hsFunName :: !Text,
    hsFunParams :: [HsFunParam],
    hsFunReturnType :: !CType
  }
  deriving (Eq, Show)

-- | A parameter to a function.
data HsFunParam = HsFunParam
  { hsFunParamName :: !Text,
    hsFunParamType :: !CType
  }
  deriving (Eq, Show)

-- | Extract a function from the C API.
hsFun ::
  -- | The C API.
  CApi ->
  -- | Name of the function to extract.
  Text ->
  -- | Haskell function.
  HsFun
hsFun cApi name =
  let cfn :: Parse.CFun
      cfn = Parse.cApiFuns cApi Map.! name

      params' :: [Parse.CFunParam]
      typ :: CType
      Parse.CFun _ typ params' = cfn

      params :: [HsFunParam]
      params = params' <&> \(Parse.CFunParam n t) -> HsFunParam n t
   in HsFun name params typ