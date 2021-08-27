{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : WGPU.Internal.Pipeline
-- Description : Pipelines.
module WGPU.Internal.Pipeline
  ( -- * Types
    PipelineLayout (..),
    PipelineLayoutDescriptor (..),
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
    StencilFaceState (..),
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
    RenderPipelineDescriptor (..),

    -- * Functions
    createPipelineLayout,
    createRenderPipeline,
    colorWriteMaskAll,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits ((.|.))
import Data.Default (Default, def)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Word (Word32, Word64, Word8)
import Foreign (nullPtr)
import Foreign.C (CFloat (CFloat))
import WGPU.Internal.Binding (BindGroupLayout)
import WGPU.Internal.Device (Device, deviceInst, wgpuDevice)
import WGPU.Internal.Instance (wgpuHsInstance)
import WGPU.Internal.Memory
  ( ToRaw,
    evalContT,
    raw,
    rawArrayPtr,
    rawPtr,
    showWithPtr,
  )
import WGPU.Internal.Multipurpose (CompareFunction)
import WGPU.Internal.RenderPass (RenderPipeline (RenderPipeline))
import WGPU.Internal.SMaybe (SMaybe (SJust, SNothing))
import WGPU.Internal.Shader (ShaderEntryPoint, ShaderModule)
import WGPU.Internal.Texture (TextureFormat)
import WGPU.Raw.Generated.Enum.WGPUBlendFactor (WGPUBlendFactor)
import qualified WGPU.Raw.Generated.Enum.WGPUBlendFactor as WGPUBlendFactor
import WGPU.Raw.Generated.Enum.WGPUBlendOperation (WGPUBlendOperation)
import qualified WGPU.Raw.Generated.Enum.WGPUBlendOperation as WGPUBlendOperation
import WGPU.Raw.Generated.Enum.WGPUColorWriteMask (WGPUColorWriteMask (WGPUColorWriteMask))
import qualified WGPU.Raw.Generated.Enum.WGPUColorWriteMask as WGPUColorWriteMask
import WGPU.Raw.Generated.Enum.WGPUCullMode (WGPUCullMode)
import qualified WGPU.Raw.Generated.Enum.WGPUCullMode as WGPUCullMode
import WGPU.Raw.Generated.Enum.WGPUFrontFace (WGPUFrontFace)
import qualified WGPU.Raw.Generated.Enum.WGPUFrontFace as WGPUFrontFace
import WGPU.Raw.Generated.Enum.WGPUIndexFormat (WGPUIndexFormat)
import qualified WGPU.Raw.Generated.Enum.WGPUIndexFormat as WGPUIndexFormat
import WGPU.Raw.Generated.Enum.WGPUInputStepMode (WGPUInputStepMode)
import qualified WGPU.Raw.Generated.Enum.WGPUInputStepMode as WGPUInputStepMode
import WGPU.Raw.Generated.Enum.WGPUPrimitiveTopology (WGPUPrimitiveTopology)
import qualified WGPU.Raw.Generated.Enum.WGPUPrimitiveTopology as WGPUPrimitiveTopology
import WGPU.Raw.Generated.Enum.WGPUStencilOperation (WGPUStencilOperation)
import qualified WGPU.Raw.Generated.Enum.WGPUStencilOperation as WGPUStencilOperation
import WGPU.Raw.Generated.Enum.WGPUVertexFormat (WGPUVertexFormat)
import qualified WGPU.Raw.Generated.Enum.WGPUVertexFormat as WGPUVertexFormat
import qualified WGPU.Raw.Generated.Fun as RawFun
import WGPU.Raw.Generated.Struct.WGPUBlendComponent (WGPUBlendComponent)
import qualified WGPU.Raw.Generated.Struct.WGPUBlendComponent as WGPUBlendComponent
import WGPU.Raw.Generated.Struct.WGPUBlendState (WGPUBlendState)
import qualified WGPU.Raw.Generated.Struct.WGPUBlendState as WGPUBlendState
import WGPU.Raw.Generated.Struct.WGPUColorTargetState (WGPUColorTargetState)
import qualified WGPU.Raw.Generated.Struct.WGPUColorTargetState as WGPUColorTargetState
import WGPU.Raw.Generated.Struct.WGPUDepthStencilState (WGPUDepthStencilState)
import qualified WGPU.Raw.Generated.Struct.WGPUDepthStencilState as WGPUDepthStencilState
import WGPU.Raw.Generated.Struct.WGPUFragmentState (WGPUFragmentState)
import qualified WGPU.Raw.Generated.Struct.WGPUFragmentState as WGPUFragmentState
import WGPU.Raw.Generated.Struct.WGPUMultisampleState (WGPUMultisampleState)
import qualified WGPU.Raw.Generated.Struct.WGPUMultisampleState as WGPUMultisampleState
import WGPU.Raw.Generated.Struct.WGPUPipelineLayoutDescriptor (WGPUPipelineLayoutDescriptor)
import qualified WGPU.Raw.Generated.Struct.WGPUPipelineLayoutDescriptor as WGPUPipelineLayoutDescriptor
import WGPU.Raw.Generated.Struct.WGPUPrimitiveState (WGPUPrimitiveState)
import qualified WGPU.Raw.Generated.Struct.WGPUPrimitiveState as WGPUPrimitiveState
import WGPU.Raw.Generated.Struct.WGPURenderPipelineDescriptor (WGPURenderPipelineDescriptor)
import qualified WGPU.Raw.Generated.Struct.WGPURenderPipelineDescriptor as WGPURenderPipelineDescriptor
import WGPU.Raw.Generated.Struct.WGPUStencilFaceState (WGPUStencilFaceState)
import qualified WGPU.Raw.Generated.Struct.WGPUStencilFaceState as WGPUStencilFaceState
import WGPU.Raw.Generated.Struct.WGPUVertexAttribute (WGPUVertexAttribute)
import qualified WGPU.Raw.Generated.Struct.WGPUVertexAttribute as WGPUVertexAttribute
import WGPU.Raw.Generated.Struct.WGPUVertexBufferLayout (WGPUVertexBufferLayout)
import qualified WGPU.Raw.Generated.Struct.WGPUVertexBufferLayout as WGPUVertexBufferLayout
import WGPU.Raw.Generated.Struct.WGPUVertexState (WGPUVertexState)
import qualified WGPU.Raw.Generated.Struct.WGPUVertexState as WGPUVertexState
import WGPU.Raw.Types (WGPUPipelineLayout (WGPUPipelineLayout))
import Prelude hiding (compare)

-------------------------------------------------------------------------------

newtype PipelineLayout = PipelineLayout {wgpuPipelineLayout :: WGPUPipelineLayout}

instance Show PipelineLayout where
  show p =
    let PipelineLayout (WGPUPipelineLayout ptr) = p
     in showWithPtr "PipelineLayout" ptr

instance Eq PipelineLayout where
  (==) p1 p2 =
    let PipelineLayout (WGPUPipelineLayout p1_ptr) = p1
        PipelineLayout (WGPUPipelineLayout p2_ptr) = p2
     in p1_ptr == p2_ptr

instance ToRaw PipelineLayout WGPUPipelineLayout where
  raw = pure . wgpuPipelineLayout

-------------------------------------------------------------------------------

-- | Describes a pipeline layout.
data PipelineLayoutDescriptor = PipelineLayoutDescriptor
  { -- | Debug label of the pipeline layout.
    pipelineLabel :: !Text,
    -- | Bind groups that this pipeline uses.
    bindGroupLayouts :: !(Vector BindGroupLayout)
  }
  deriving (Eq, Show)

instance ToRaw PipelineLayoutDescriptor WGPUPipelineLayoutDescriptor where
  raw PipelineLayoutDescriptor {..} = do
    label_ptr <- rawPtr pipelineLabel
    bindGroupLayouts_ptr <- rawArrayPtr bindGroupLayouts
    pure
      WGPUPipelineLayoutDescriptor.WGPUPipelineLayoutDescriptor
        { nextInChain = nullPtr,
          label = label_ptr,
          bindGroupLayoutCount = fromIntegral . Vector.length $ bindGroupLayouts,
          bindGroupLayouts = bindGroupLayouts_ptr
        }

-------------------------------------------------------------------------------

-- | Create a pipeline layout.
createPipelineLayout ::
  MonadIO m =>
  -- | The device for which the pipeline layout will be created.
  Device ->
  -- | Descriptor of the pipeline.
  PipelineLayoutDescriptor ->
  m PipelineLayout
createPipelineLayout device pdl = liftIO . evalContT $ do
  let inst = deviceInst device
  pipelineLayoutDescriptor_ptr <- rawPtr pdl
  rawPipelineLayout <-
    RawFun.wgpuDeviceCreatePipelineLayout
      (wgpuHsInstance inst)
      (wgpuDevice device)
      pipelineLayoutDescriptor_ptr
  pure (PipelineLayout rawPipelineLayout)

-------------------------------------------------------------------------------

-- | Vertex format for a vertex attribute.
data VertexFormat
  = VertexFormatUint8x2
  | VertexFormatUint8x4
  | VertexFormatSint8x2
  | VertexFormatSint8x4
  | VertexFormatUnorm8x2
  | VertexFormatUnorm8x4
  | VertexFormatSnorm8x2
  | VertexFormatSnorm8x4
  | VertexFormatUint16x2
  | VertexFormatUint16x4
  | VertexFormatSint16x2
  | VertexFormatSint16x4
  | VertexFormatUnorm16x2
  | VertexFormatUnorm16x4
  | VertexFormatSnorm16x2
  | VertexFormatSnorm16x4
  | VertexFormatFloat16x2
  | VertexFormatFloat16x4
  | VertexFormatFloat32
  | VertexFormatFloat32x2
  | VertexFormatFloat32x3
  | VertexFormatFloat32x4
  | VertexFormatUint32
  | VertexFormatUint32x2
  | VertexFormatUint32x3
  | VertexFormatUint32x4
  | VertexFormatSint32
  | VertexFormatSint32x2
  | VertexFormatSint32x3
  | VertexFormatSint32x4
  deriving (Eq, Show)

-- | Convert a 'VertexFormat' to its raw representation.
instance ToRaw VertexFormat WGPUVertexFormat where
  raw vf =
    pure $
      case vf of
        VertexFormatUint8x2 -> WGPUVertexFormat.Uint8x2
        VertexFormatUint8x4 -> WGPUVertexFormat.Uint8x4
        VertexFormatSint8x2 -> WGPUVertexFormat.Sint8x2
        VertexFormatSint8x4 -> WGPUVertexFormat.Sint8x4
        VertexFormatUnorm8x2 -> WGPUVertexFormat.Unorm8x2
        VertexFormatUnorm8x4 -> WGPUVertexFormat.Unorm8x4
        VertexFormatSnorm8x2 -> WGPUVertexFormat.Snorm8x2
        VertexFormatSnorm8x4 -> WGPUVertexFormat.Snorm8x4
        VertexFormatUint16x2 -> WGPUVertexFormat.Uint16x2
        VertexFormatUint16x4 -> WGPUVertexFormat.Uint16x4
        VertexFormatSint16x2 -> WGPUVertexFormat.Sint16x2
        VertexFormatSint16x4 -> WGPUVertexFormat.Sint16x4
        VertexFormatUnorm16x2 -> WGPUVertexFormat.Unorm16x2
        VertexFormatUnorm16x4 -> WGPUVertexFormat.Unorm16x4
        VertexFormatSnorm16x2 -> WGPUVertexFormat.Snorm16x2
        VertexFormatSnorm16x4 -> WGPUVertexFormat.Snorm16x4
        VertexFormatFloat16x2 -> WGPUVertexFormat.Float16x2
        VertexFormatFloat16x4 -> WGPUVertexFormat.Float16x4
        VertexFormatFloat32 -> WGPUVertexFormat.Float32
        VertexFormatFloat32x2 -> WGPUVertexFormat.Float32x2
        VertexFormatFloat32x3 -> WGPUVertexFormat.Float32x3
        VertexFormatFloat32x4 -> WGPUVertexFormat.Float32x4
        VertexFormatUint32 -> WGPUVertexFormat.Uint32
        VertexFormatUint32x2 -> WGPUVertexFormat.Uint32x2
        VertexFormatUint32x3 -> WGPUVertexFormat.Uint32x3
        VertexFormatUint32x4 -> WGPUVertexFormat.Uint32x4
        VertexFormatSint32 -> WGPUVertexFormat.Sint32
        VertexFormatSint32x2 -> WGPUVertexFormat.Sint32x2
        VertexFormatSint32x3 -> WGPUVertexFormat.Sint32x3
        VertexFormatSint32x4 -> WGPUVertexFormat.Sint32x4

-------------------------------------------------------------------------------

-- | Vertex inputs (attributes) to shaders.
data VertexAttribute = VertexAttribute
  { -- | Format of the input.
    vertexFormat :: !VertexFormat,
    -- | Byte offset of the start of the input.
    offset :: !Word64,
    -- | Location for this input. Must match the location in the shader.
    shaderLocation :: !Word32
  }
  deriving (Eq, Show)

instance ToRaw VertexAttribute WGPUVertexAttribute where
  raw VertexAttribute {..} = do
    n_format <- raw vertexFormat
    pure
      WGPUVertexAttribute.WGPUVertexAttribute
        { format = n_format,
          offset = offset,
          shaderLocation = shaderLocation
        }

-------------------------------------------------------------------------------

-- | Determines when vertex data is advanced.
data InputStepMode
  = -- | Input data is advanced every vertex.
    InputStepModeVertex
  | -- | Input data is advanced every instance.
    InputStepModeInstance
  deriving (Eq, Show)

-- | Convert an 'InputStepMode' to its raw value.
instance ToRaw InputStepMode WGPUInputStepMode where
  raw ism =
    pure $
      case ism of
        InputStepModeVertex -> WGPUInputStepMode.Vertex
        InputStepModeInstance -> WGPUInputStepMode.Instance

-------------------------------------------------------------------------------

-- | Describes how a vertex buffer is interpreted.
data VertexBufferLayout = VertexBufferLayout
  { -- | The stride, in bytes, between elements of the buffer.
    arrayStride :: !Word64,
    -- | How often the vertex buffer is stepped forward (per vertex or
    -- per instance).
    stepMode :: !InputStepMode,
    -- | List of attributes that comprise a single vertex.
    attributes :: !(Vector VertexAttribute)
  }
  deriving (Eq, Show)

instance ToRaw VertexBufferLayout WGPUVertexBufferLayout where
  raw VertexBufferLayout {..} = do
    n_stepMode <- raw stepMode
    attributes_ptr <- rawArrayPtr attributes
    pure
      WGPUVertexBufferLayout.WGPUVertexBufferLayout
        { arrayStride = arrayStride,
          stepMode = n_stepMode,
          attributeCount = fromIntegral . length $ attributes,
          attributes = attributes_ptr
        }

-------------------------------------------------------------------------------

-- | Describes the vertex process in a render pipeline.
data VertexState = VertexState
  { -- | The compiled shader module for this stage.
    vertexShaderModule :: !ShaderModule,
    -- | The name of the entry point in the compiled shader. There must be a
    -- function that returns @void@ with this name in the shader.
    vertexEntryPoint :: !ShaderEntryPoint,
    -- | The format of any vertex buffers used with this pipeline.
    buffers :: !(Vector VertexBufferLayout)
  }
  deriving (Eq, Show)

instance ToRaw VertexState WGPUVertexState where
  raw VertexState {..} = do
    n_shaderModule <- raw vertexShaderModule
    entryPoint_ptr <- rawPtr vertexEntryPoint
    buffers_ptr <- rawArrayPtr buffers
    pure
      WGPUVertexState.WGPUVertexState
        { nextInChain = nullPtr,
          shaderModule = n_shaderModule,
          entryPoint = entryPoint_ptr,
          bufferCount = fromIntegral . length $ buffers,
          buffers = buffers_ptr
        }

-------------------------------------------------------------------------------

-- | Primitive type out of which an input mesh is composed.
data PrimitiveTopology
  = PrimitiveTopologyPointList
  | PrimitiveTopologyLineList
  | PrimitiveTopologyLineStrip
  | PrimitiveTopologyTriangleList
  | PrimitiveTopologyTriangleStrip
  deriving (Eq, Show)

instance Default PrimitiveTopology where def = PrimitiveTopologyTriangleList

-- | Convert a 'PrimitiveTopology' to its raw value.
instance ToRaw PrimitiveTopology WGPUPrimitiveTopology where
  raw pt =
    pure $
      case pt of
        PrimitiveTopologyPointList -> WGPUPrimitiveTopology.PointList
        PrimitiveTopologyLineList -> WGPUPrimitiveTopology.LineList
        PrimitiveTopologyLineStrip -> WGPUPrimitiveTopology.LineStrip
        PrimitiveTopologyTriangleList -> WGPUPrimitiveTopology.TriangleList
        PrimitiveTopologyTriangleStrip -> WGPUPrimitiveTopology.TriangleStrip

-------------------------------------------------------------------------------

-- | Format of indices used within a pipeline.
data IndexFormat
  = -- | Indices are 16-bit unsigned integers ('Word16')
    IndexFormatUint16
  | -- | Indices are 32-bit unsigned integers ('Word32')
    IndexFormatUint32
  deriving (Eq, Show)

-- | Convert an 'IndexFormat' to its raw value.
instance ToRaw IndexFormat WGPUIndexFormat where
  raw idxFmt =
    pure $
      case idxFmt of
        IndexFormatUint16 -> WGPUIndexFormat.Uint16
        IndexFormatUint32 -> WGPUIndexFormat.Uint32

-------------------------------------------------------------------------------

-- | Winding order which classifies the "front" face.
data FrontFace
  = -- | Triangles with counter-clockwise vertices are the front face.
    FrontFaceCCW
  | -- | Triangles with clockwise vertices are the front face.
    FrontFaceCW
  deriving (Eq, Show)

instance Default FrontFace where def = FrontFaceCCW

-- | Convert a 'FrontFace' to its raw value.
instance ToRaw FrontFace WGPUFrontFace where
  raw ff =
    pure $
      case ff of
        FrontFaceCCW -> WGPUFrontFace.CCW
        FrontFaceCW -> WGPUFrontFace.CW

-------------------------------------------------------------------------------

-- | Whether to cull the face of a vertex.
data CullMode
  = -- | Cull the front face.
    CullModeFront
  | -- | Cull the back face.
    CullModeBack
  | -- | Do not cull either face.
    CullModeNone
  deriving (Eq, Show)

instance Default CullMode where def = CullModeNone

-- | Convert a 'CullMode' to its raw value.
instance ToRaw CullMode WGPUCullMode where
  raw cm =
    pure $
      case cm of
        CullModeFront -> WGPUCullMode.Front
        CullModeBack -> WGPUCullMode.Back
        CullModeNone -> WGPUCullMode.None

-------------------------------------------------------------------------------

-- | Describes the state of primitive assembly and rasterization in a render
-- pipeline.
--
-- Differences between this and the Rust API:
--   - no `clamp_depth` member
--   - no `polygon_mode` member
--   - no `conservative` member
data PrimitiveState = PrimitiveState
  { -- | The primitive topology used to interpret vertices.
    topology :: !PrimitiveTopology,
    -- | When drawing strip topologies with indices, this is the required
    -- format for the index buffer. This has no effect for non-indexed or
    -- non-strip draws.
    stripIndexFormat :: !(SMaybe IndexFormat),
    -- | The face to consider the front for the purpose of culling and
    -- stencil operations.
    frontFace :: !FrontFace,
    -- | The face culling mode.
    cullMode :: !CullMode
  }
  deriving (Eq, Show)

instance Default PrimitiveState where
  def =
    PrimitiveState
      { topology = def,
        stripIndexFormat = SNothing,
        frontFace = def,
        cullMode = def
      }

instance ToRaw PrimitiveState WGPUPrimitiveState where
  raw PrimitiveState {..} = do
    n_topology <- raw topology
    n_stripIndexFormat <-
      case stripIndexFormat of
        SNothing -> pure WGPUIndexFormat.Undefined
        SJust sif -> raw sif
    n_frontFace <- raw frontFace
    n_cullMode <- raw cullMode
    pure
      WGPUPrimitiveState.WGPUPrimitiveState
        { nextInChain = nullPtr,
          topology = n_topology,
          stripIndexFormat = n_stripIndexFormat,
          frontFace = n_frontFace,
          cullMode = n_cullMode
        }

-------------------------------------------------------------------------------

-- | Operation to perform on a stencil value.
data StencilOperation
  = StencilOperationKeep
  | StencilOperationZero
  | StencilOperationReplace
  | StencilOperationInvert
  | StencilOperationIncrementClamp
  | StencilOperationDecrementClamp
  | StencilOperationIncrementWrap
  | StencilOperationDecrementWrap
  deriving (Eq, Show)

-- | Convert a 'StencilOperation' to its raw value.
instance ToRaw StencilOperation WGPUStencilOperation where
  raw so =
    pure $
      case so of
        StencilOperationKeep -> WGPUStencilOperation.Keep
        StencilOperationZero -> WGPUStencilOperation.Zero
        StencilOperationReplace -> WGPUStencilOperation.Replace
        StencilOperationInvert -> WGPUStencilOperation.Invert
        StencilOperationIncrementClamp -> WGPUStencilOperation.IncrementClamp
        StencilOperationDecrementClamp -> WGPUStencilOperation.DecrementClamp
        StencilOperationIncrementWrap -> WGPUStencilOperation.IncrementWrap
        StencilOperationDecrementWrap -> WGPUStencilOperation.DecrementWrap

-------------------------------------------------------------------------------

-- | Describes stencil state in a render pipeline.
data StencilFaceState = StencilFaceState
  { compare :: !CompareFunction,
    failOp :: !StencilOperation,
    depthFailOp :: !StencilOperation,
    passOp :: !StencilOperation
  }
  deriving (Eq, Show)

instance ToRaw StencilFaceState WGPUStencilFaceState where
  raw StencilFaceState {..} = do
    n_compare <- raw compare
    n_failOp <- raw failOp
    n_depthFailOp <- raw depthFailOp
    n_passOp <- raw passOp
    pure
      WGPUStencilFaceState.WGPUStencilFaceState
        { compare = n_compare,
          failOp = n_failOp,
          depthFailOp = n_depthFailOp,
          passOp = n_passOp
        }

-------------------------------------------------------------------------------

-- | State of the stencil operation (fixed pipeline stage).
data StencilState = StencilState
  { -- | Front face mode.
    front :: !StencilFaceState,
    -- | Back face mode.
    back :: !StencilFaceState,
    -- | Stencil values are AND-ed with this mask when reading and writing from
    -- the stencil buffer.
    readMask :: !Word8,
    -- | Stencil values are AND-ed with this mask when writing to the stencil
    -- buffer.
    writeMask :: !Word8
  }
  deriving (Eq, Show)

-------------------------------------------------------------------------------

-- | Describes the biasing setting for the depth target.
data DepthBiasState = DepthBiasState
  { -- Constant depth biasing factor, in basic units of the depth format.
    constant :: !Int32,
    -- | Slope depth biasing factor.
    slopeScale :: !Float,
    -- | Depth bias clamp value (absolute).
    clamp :: !Float
  }
  deriving (Eq, Show)

-------------------------------------------------------------------------------

-- | Describes the depth / stencil state of a render pipeline.
data DepthStencilState = DepthStencilState
  { -- | Format of the depth/stencil buffer. This must be a special depth
    -- format, and must match the format of the depth/stencil attachment in
    -- the command encoder.
    depthStencilTextureFormat :: !TextureFormat,
    -- | If disabled, depth will not be written to.
    depthWriteEnabled :: !Bool,
    -- | Comparison function used to compare depth values in the depth test.
    depthCompare :: !CompareFunction,
    -- | Stencil state.
    stencil :: !StencilState,
    -- | Depth bias state.
    bias :: !DepthBiasState
  }
  deriving (Eq, Show)

instance ToRaw DepthStencilState WGPUDepthStencilState where
  raw DepthStencilState {..} = do
    n_format <- raw depthStencilTextureFormat
    n_depthWriteEnabled <- raw depthWriteEnabled
    n_depthCompare <- raw depthCompare
    n_stencilFront <- raw . front $ stencil
    n_stencilBack <- raw . back $ stencil
    pure
      WGPUDepthStencilState.WGPUDepthStencilState
        { nextInChain = nullPtr,
          format = n_format,
          depthWriteEnabled = n_depthWriteEnabled,
          depthCompare = n_depthCompare,
          stencilFront = n_stencilFront,
          stencilBack = n_stencilBack,
          stencilReadMask = fromIntegral . readMask $ stencil,
          stencilWriteMask = fromIntegral $ writeMask (stencil :: StencilState),
          depthBias = constant bias,
          depthBiasSlopeScale = CFloat (slopeScale bias),
          depthBiasClamp = CFloat (clamp bias)
        }

-------------------------------------------------------------------------------

-- | Describes the multi-sampling state of a render pipeline.
data MultisampleState = MultisampleState
  { -- | Number of samples calculated per pixel (for MSAA). For
    -- non-multisampled textures, this should be 1.
    count :: Word32,
    -- | Bitmask that restricts the samples of a pixel modified by this
    -- pipeline. All samples can be enabled by using
    -- @0XFFFFFFFF@ (ie. zero complement).
    mask :: Word32,
    -- | When enabled, produces another sample mask per pixel based on the
    -- alpha output value, and that is AND-ed with the sample mask and the
    -- primitive coverage to restrict the set of samples affected by a
    -- primitive.
    alphaToCoverageEnabled :: Bool
  }
  deriving (Eq, Show)

instance ToRaw MultisampleState WGPUMultisampleState where
  raw MultisampleState {..} = do
    n_alphaToCoverageEnabled <- raw alphaToCoverageEnabled
    pure
      WGPUMultisampleState.WGPUMultisampleState
        { nextInChain = nullPtr,
          count = count,
          mask = mask,
          alphaToCoverageEnabled = n_alphaToCoverageEnabled
        }

-------------------------------------------------------------------------------

-- | Alpha blend factor.
data BlendFactor
  = BlendFactorZero
  | BlendFactorOne
  | BlendFactorSrc
  | BlendFactorOneMinusSrc
  | BlendFactorSrcAlpha
  | BlendFactorOneMinusSrcAlpha
  | BlendFactorDst
  | BlendFactorOneMinusDst
  | BlendFactorDstAlpha
  | BlendFactorOneMinusDstAlpha
  | BlendFactorSrcAlphaSaturated
  | BlendFactorConstant
  | BlendFactorOneMinusConstant
  deriving (Eq, Show)

-- | Convert a 'BlendFactor' to its raw value.
instance ToRaw BlendFactor WGPUBlendFactor where
  raw bf =
    pure $
      case bf of
        BlendFactorZero -> WGPUBlendFactor.Zero
        BlendFactorOne -> WGPUBlendFactor.One
        BlendFactorSrc -> WGPUBlendFactor.Src
        BlendFactorOneMinusSrc -> WGPUBlendFactor.OneMinusSrc
        BlendFactorSrcAlpha -> WGPUBlendFactor.SrcAlpha
        BlendFactorOneMinusSrcAlpha -> WGPUBlendFactor.OneMinusSrcAlpha
        BlendFactorDst -> WGPUBlendFactor.Dst
        BlendFactorOneMinusDst -> WGPUBlendFactor.OneMinusDst
        BlendFactorDstAlpha -> WGPUBlendFactor.DstAlpha
        BlendFactorOneMinusDstAlpha -> WGPUBlendFactor.OneMinusDstAlpha
        BlendFactorSrcAlphaSaturated -> WGPUBlendFactor.SrcAlphaSaturated
        BlendFactorConstant -> WGPUBlendFactor.Constant
        BlendFactorOneMinusConstant -> WGPUBlendFactor.OneMinusConstant

-------------------------------------------------------------------------------

-- | Alpha blending operation.
data BlendOperation
  = BlendOperationAdd
  | BlendOperationSubtract
  | BlendOperationReverseSubtract
  | BlendOperationMin
  | BlendOperationMax
  deriving (Eq, Show)

-- | Convert a 'BlendOperation' to its raw value.
instance ToRaw BlendOperation WGPUBlendOperation where
  raw bo =
    pure $
      case bo of
        BlendOperationAdd -> WGPUBlendOperation.Add
        BlendOperationSubtract -> WGPUBlendOperation.Subtract
        BlendOperationReverseSubtract -> WGPUBlendOperation.ReverseSubtract
        BlendOperationMin -> WGPUBlendOperation.Min
        BlendOperationMax -> WGPUBlendOperation.Max

-------------------------------------------------------------------------------

-- | Describes the blend component of a pipeline.
data BlendComponent = BlendComponent
  { -- | Multiplier for the source, which is produced by the fragment shader.
    srcFactor :: !BlendFactor,
    -- | Multiplier for the destination, which is stored in the target.
    dstFactor :: !BlendFactor,
    -- | Binary operation applied to the source and destination, multiplied by
    -- their respective factors.
    operation :: !BlendOperation
  }
  deriving (Eq, Show)

instance Default BlendComponent where
  def =
    BlendComponent
      { srcFactor = BlendFactorOne,
        dstFactor = BlendFactorZero,
        operation = BlendOperationAdd
      }

instance ToRaw BlendComponent WGPUBlendComponent where
  raw BlendComponent {..} = do
    n_srcFactor <- raw srcFactor
    n_dstFactor <- raw dstFactor
    n_operation <- raw operation
    pure
      WGPUBlendComponent.WGPUBlendComponent
        { srcFactor = n_srcFactor,
          dstFactor = n_dstFactor,
          operation = n_operation
        }

-------------------------------------------------------------------------------

-- | Describes the blend state of a render pipeline.
data BlendState = BlendState
  { -- | Color equation.
    blendColor :: !BlendComponent,
    -- | Alpha equation.
    blendAlpha :: !BlendComponent
  }
  deriving (Eq, Show)

instance ToRaw BlendState WGPUBlendState where
  raw BlendState {..} = do
    n_color <- raw blendColor
    n_alpha <- raw blendAlpha
    pure
      WGPUBlendState.WGPUBlendState
        { color = n_color,
          alpha = n_alpha
        }

-------------------------------------------------------------------------------

-- | Describes which color channels are written.
data ColorWriteMask = ColorWriteMask
  { maskRed :: !Bool,
    maskGreen :: !Bool,
    maskBlue :: !Bool,
    maskAlpha :: !Bool
  }
  deriving (Eq, Show)

-- | A 'ColorWriteMask' that writes all colors and the alpha value.
colorWriteMaskAll :: ColorWriteMask
colorWriteMaskAll = ColorWriteMask True True True True

instance ToRaw ColorWriteMask WGPUColorWriteMask where
  raw ColorWriteMask {..} =
    pure $
      WGPUColorWriteMask
        ( (if maskRed then WGPUColorWriteMask.Red else 0)
            .|. (if maskGreen then WGPUColorWriteMask.Green else 0)
            .|. (if maskBlue then WGPUColorWriteMask.Blue else 0)
            .|. (if maskAlpha then WGPUColorWriteMask.Alpha else 0)
        )

-------------------------------------------------------------------------------

-- | Describes the color state of a render pipeline.
data ColorTargetState = ColorTargetState
  { -- | The texture format of the image that this pipeline will render to.
    -- Must match the format of the corresponding color attachment in the
    -- command encoder.
    colorTextureFormat :: !TextureFormat,
    -- | The blending that is used for this pipeline.
    blend :: !(SMaybe BlendState),
    -- | Mask which enables or disables writes to different color/alpha
    -- channels.
    colorWriteMask :: !ColorWriteMask
  }
  deriving (Eq, Show)

instance ToRaw ColorTargetState WGPUColorTargetState where
  raw ColorTargetState {..} = do
    n_format <- raw colorTextureFormat
    blend_ptr <-
      case blend of
        SNothing -> pure nullPtr
        SJust x -> rawPtr x
    WGPUColorWriteMask n_writeMask <- raw colorWriteMask
    pure
      WGPUColorTargetState.WGPUColorTargetState
        { nextInChain = nullPtr,
          format = n_format,
          blend = blend_ptr,
          writeMask = n_writeMask
        }

-------------------------------------------------------------------------------

-- | Describes the fragment processing in a render pipeline.
data FragmentState = FragmentState
  { -- | The compiled shader module for this stage.
    fragmentShaderModule :: !ShaderModule,
    -- | The entry point in the compiled shader. There must be a function that
    -- returns @void@ with this name in the shader.
    fragmentEntryPoint :: !ShaderEntryPoint,
    -- | The color state of the render targets.
    targets :: !(Vector ColorTargetState)
  }
  deriving (Eq, Show)

instance ToRaw FragmentState WGPUFragmentState where
  raw FragmentState {..} = do
    n_shaderModule <- raw fragmentShaderModule
    entryPoint_ptr <- rawPtr fragmentEntryPoint
    targets_ptr <- rawArrayPtr targets
    pure
      WGPUFragmentState.WGPUFragmentState
        { nextInChain = nullPtr,
          shaderModule = n_shaderModule,
          entryPoint = entryPoint_ptr,
          targetCount = fromIntegral . length $ targets,
          targets = targets_ptr
        }

-------------------------------------------------------------------------------

-- | Describes a render (graphics) pipeline.
data RenderPipelineDescriptor = RenderPipelineDescriptor
  { -- | Debug label of the pipeline.
    renderPipelineLabel :: !Text,
    -- | The layout of bind groups for this pipeline.
    layout :: !(SMaybe PipelineLayout),
    -- | Vertex state.
    vertex :: !VertexState,
    -- | Primitive state.
    primitive :: !PrimitiveState,
    -- | Depth stencil state.
    depthStencil :: !(SMaybe DepthStencilState),
    -- | Multisample state.
    multisample :: !MultisampleState,
    -- | Fragment state.
    fragment :: !(SMaybe FragmentState)
  }
  deriving (Eq, Show)

instance ToRaw RenderPipelineDescriptor WGPURenderPipelineDescriptor where
  raw RenderPipelineDescriptor {..} = do
    label_ptr <- rawPtr renderPipelineLabel
    n_layout <-
      case layout of
        SNothing -> pure (WGPUPipelineLayout nullPtr)
        SJust x -> raw x
    n_vertex <- raw vertex
    n_primitive <- raw primitive
    n_depthStencil <-
      case depthStencil of
        SNothing -> pure nullPtr
        SJust x -> rawPtr x
    n_multisample <- raw multisample
    n_fragment <-
      case fragment of
        SNothing -> pure nullPtr
        SJust x -> rawPtr x
    pure
      WGPURenderPipelineDescriptor.WGPURenderPipelineDescriptor
        { nextInChain = nullPtr,
          label = label_ptr,
          layout = n_layout,
          vertex = n_vertex,
          primitive = n_primitive,
          depthStencil = n_depthStencil,
          multisample = n_multisample,
          fragment = n_fragment
        }

-------------------------------------------------------------------------------

createRenderPipeline ::
  MonadIO m =>
  Device ->
  RenderPipelineDescriptor ->
  m RenderPipeline
createRenderPipeline device rpd = liftIO . evalContT $ do
  let inst = deviceInst device
  renderPipelineDescriptor_ptr <- rawPtr rpd
  renderPipelineRaw <-
    RawFun.wgpuDeviceCreateRenderPipeline
      (wgpuHsInstance inst)
      (wgpuDevice device)
      renderPipelineDescriptor_ptr
  pure (RenderPipeline renderPipelineRaw)
