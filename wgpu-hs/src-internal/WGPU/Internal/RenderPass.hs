{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : WGPU.Internal.RenderPass
-- Description : Render passes.
module WGPU.Internal.RenderPass
  ( -- * Types
    RenderPipeline (..),
    RenderPassEncoder,
    LoadOp (..),
    StoreOp (..),
    Operations (..),
    RenderPassColorAttachment (..),
    RenderPassDepthStencilAttachment (..),
    RenderPassDescriptor (..),
    Range (..),

    -- * Functions
    beginRenderPass,
    renderPassSetPipeline,
    renderPassDraw,
    endRenderPass,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word32)
import Foreign (nullPtr)
import Foreign.C (CBool (CBool), CFloat (CFloat))
import WGPU.Internal.Color (Color, transparentBlack)
import WGPU.Internal.CommandEncoder
  ( CommandEncoder,
    commandEncoderInst,
    wgpuCommandEncoder,
  )
import WGPU.Internal.Instance (Instance, wgpuHsInstance)
import WGPU.Internal.Memory
  ( ToRaw,
    evalContT,
    raw,
    rawArrayPtr,
    rawPtr,
    showWithPtr,
  )
import WGPU.Internal.SMaybe (SMaybe (SJust, SNothing))
import WGPU.Internal.Texture (TextureView)
import qualified WGPU.Raw.Generated.Enum.WGPULoadOp as WGPULoadOp
import WGPU.Raw.Generated.Enum.WGPUStoreOp (WGPUStoreOp)
import qualified WGPU.Raw.Generated.Enum.WGPUStoreOp as WGPUStoreOp
import qualified WGPU.Raw.Generated.Fun as RawFun
import WGPU.Raw.Generated.Struct.WGPURenderPassColorAttachment (WGPURenderPassColorAttachment)
import qualified WGPU.Raw.Generated.Struct.WGPURenderPassColorAttachment as WGPURenderPassColorAttachment
import WGPU.Raw.Generated.Struct.WGPURenderPassDepthStencilAttachment (WGPURenderPassDepthStencilAttachment)
import qualified WGPU.Raw.Generated.Struct.WGPURenderPassDepthStencilAttachment as WGPURenderPassDepthStencilAttachment
import WGPU.Raw.Generated.Struct.WGPURenderPassDescriptor (WGPURenderPassDescriptor)
import qualified WGPU.Raw.Generated.Struct.WGPURenderPassDescriptor as WGPURenderPassDescriptor
import WGPU.Raw.Types
  ( WGPUQuerySet (WGPUQuerySet),
    WGPURenderPassEncoder (WGPURenderPassEncoder),
    WGPURenderPipeline (WGPURenderPipeline),
    WGPUTextureView (WGPUTextureView),
  )

-------------------------------------------------------------------------------

newtype RenderPipeline = RenderPipeline {wgpuRenderPipeline :: WGPURenderPipeline}

instance Show RenderPipeline where
  show p =
    let RenderPipeline (WGPURenderPipeline ptr) = p
     in showWithPtr "RenderPipeline" ptr

instance Eq RenderPipeline where
  (==) p1 p2 =
    let RenderPipeline (WGPURenderPipeline p1_ptr) = p1
        RenderPipeline (WGPURenderPipeline p2_ptr) = p2
     in p1_ptr == p2_ptr

instance ToRaw RenderPipeline WGPURenderPipeline where
  raw = pure . wgpuRenderPipeline

-------------------------------------------------------------------------------

data RenderPassEncoder = RenderPassEncoder
  { renderPassEncoderInst :: !Instance,
    wgpuRenderPassEncoder :: !WGPURenderPassEncoder
  }

instance Show RenderPassEncoder where
  show e =
    let RenderPassEncoder _ (WGPURenderPassEncoder ptr) = e
     in showWithPtr "RenderPassEncoder" ptr

instance Eq RenderPassEncoder where
  (==) e1 e2 =
    let RenderPassEncoder _ (WGPURenderPassEncoder e1_ptr) = e1
        RenderPassEncoder _ (WGPURenderPassEncoder e2_ptr) = e2
     in e1_ptr == e2_ptr

instance ToRaw RenderPassEncoder WGPURenderPassEncoder where
  raw = pure . wgpuRenderPassEncoder

-------------------------------------------------------------------------------

-- | Operation to perform to the output attachment at the start of a render
-- pass.
data LoadOp a
  = -- | Clear with the specified color value.
    LoadOpClear !a
  | -- | Load from memory.
    LoadOpLoad
  deriving (Eq, Show)

-------------------------------------------------------------------------------

-- | Operation to perform to the output attachment at the end of the render
-- pass.
data StoreOp
  = -- | Store the result.
    StoreOpStore
  | -- | Discard the result.
    StoreOpClear
  deriving (Eq, Show)

instance ToRaw StoreOp WGPUStoreOp where
  raw storeOp =
    pure $
      case storeOp of
        StoreOpStore -> WGPUStoreOp.Store
        StoreOpClear -> WGPUStoreOp.Clear

-------------------------------------------------------------------------------

data Operations a = Operations
  { load :: !(LoadOp a),
    store :: !StoreOp
  }
  deriving (Eq, Show)

-------------------------------------------------------------------------------

-- | Describes a color attachment to a render pass.
data RenderPassColorAttachment = RenderPassColorAttachment
  { -- | The view to use as an attachment.
    colorView :: !TextureView,
    -- | The view that will receive output if multisampling is used.
    resolveTarget :: !(SMaybe TextureView),
    -- | What operations will be performed on this color attachment.
    operations :: !(Operations Color)
  }
  deriving (Eq, Show)

instance ToRaw RenderPassColorAttachment WGPURenderPassColorAttachment where
  raw RenderPassColorAttachment {..} = do
    n_view <- raw colorView
    n_resolveTarget <-
      case resolveTarget of
        SNothing -> pure (WGPUTextureView nullPtr)
        SJust t -> raw t
    n_storeOp <- raw . store $ operations
    (n_loadOp, n_clearColor) <-
      case load operations of
        LoadOpClear color -> do
          n_color <- raw color
          pure (WGPULoadOp.Clear, n_color)
        LoadOpLoad -> do
          n_color <- raw transparentBlack
          pure (WGPULoadOp.Load, n_color)
    pure
      WGPURenderPassColorAttachment.WGPURenderPassColorAttachment
        { view = n_view,
          resolveTarget = n_resolveTarget,
          loadOp = n_loadOp,
          storeOp = n_storeOp,
          clearColor = n_clearColor
        }

-------------------------------------------------------------------------------

-- | Describes a depth/stencil attachment to a render pass.
data RenderPassDepthStencilAttachment = RenderPassDepthStencilAttachment
  { -- | The view to use as an attachment.
    depthStencilView :: !TextureView,
    -- | What operations will be performed on the depth part.
    depthOps :: !(SMaybe (Operations Float)),
    -- | What operations will be performed on the stencil part.
    stencilOps :: !(SMaybe (Operations Word32))
  }
  deriving (Eq, Show)

instance
  ToRaw
    RenderPassDepthStencilAttachment
    WGPURenderPassDepthStencilAttachment
  where
  raw RenderPassDepthStencilAttachment {..} = do
    n_view <- raw depthStencilView

    (n_depthLoadOp, n_depthStoreOp, n_clearDepth, n_depthReadOnly) <-
      case depthOps of
        SNothing ->
          pure
            ( WGPULoadOp.Clear,
              WGPUStoreOp.Clear,
              CFloat 0,
              CBool 1
            )
        SJust Operations {..} -> do
          (loadOp, depth) <-
            case load of
              LoadOpClear d -> pure (WGPULoadOp.Clear, CFloat d)
              LoadOpLoad -> pure (WGPULoadOp.Load, CFloat 0)
          storeOp <- raw store
          pure (loadOp, storeOp, depth, CBool 0)

    (n_stencilLoadOp, n_stencilStoreOp, n_clearStencil, n_stencilReadOnly) <-
      case stencilOps of
        SNothing ->
          pure
            ( WGPULoadOp.Clear,
              WGPUStoreOp.Clear,
              0,
              CBool 1
            )
        SJust Operations {..} -> do
          (loadOp, stencil) <-
            case load of
              LoadOpClear s -> pure (WGPULoadOp.Clear, s)
              LoadOpLoad -> pure (WGPULoadOp.Load, 0)
          storeOp <- raw store
          pure (loadOp, storeOp, stencil, CBool 0)

    pure
      WGPURenderPassDepthStencilAttachment.WGPURenderPassDepthStencilAttachment
        { view = n_view,
          depthLoadOp = n_depthLoadOp,
          depthStoreOp = n_depthStoreOp,
          clearDepth = n_clearDepth,
          depthReadOnly = n_depthReadOnly,
          stencilLoadOp = n_stencilLoadOp,
          stencilStoreOp = n_stencilStoreOp,
          clearStencil = n_clearStencil,
          stencilReadOnly = n_stencilReadOnly
        }

-------------------------------------------------------------------------------

-- | Describes the attachments of a render pass.
data RenderPassDescriptor = RenderPassDescriptor
  { -- | Debugging label for the render pass.
    renderPassLabel :: !Text,
    -- | Color attachments of the render pass.
    colorAttachments :: !(Vector RenderPassColorAttachment),
    -- | Depth and stencil attachments of the render pass.
    depthStencilAttachment :: !(SMaybe RenderPassDepthStencilAttachment)
  }
  deriving (Eq, Show)

instance ToRaw RenderPassDescriptor WGPURenderPassDescriptor where
  raw RenderPassDescriptor {..} = do
    label_ptr <- rawPtr renderPassLabel
    colorAttachments_ptr <- rawArrayPtr colorAttachments
    depthStencilAttachment_ptr <-
      case depthStencilAttachment of
        SNothing -> pure nullPtr
        SJust x -> rawPtr x
    pure
      WGPURenderPassDescriptor.WGPURenderPassDescriptor
        { nextInChain = nullPtr,
          label = label_ptr,
          colorAttachmentCount = fromIntegral . length $ colorAttachments,
          colorAttachments = colorAttachments_ptr,
          depthStencilAttachment = depthStencilAttachment_ptr,
          occlusionQuerySet = WGPUQuerySet nullPtr
        }

-------------------------------------------------------------------------------

-- | Half open range. It includes the 'start' value but not the 'end' value.
data Range a = Range
  { rangeStart :: !a,
    rangeLength :: !a
  }
  deriving (Eq, Show)

-------------------------------------------------------------------------------

-- | Begins recording of a render pass.
beginRenderPass ::
  MonadIO m =>
  -- | @CommandEncoder@ to contain the render pass.
  CommandEncoder ->
  -- | Description of the render pass.
  RenderPassDescriptor ->
  -- | IO action which returns the render pass encoder.
  m RenderPassEncoder
beginRenderPass commandEncoder rpd = liftIO . evalContT $ do
  let inst = commandEncoderInst commandEncoder
  renderPassDescriptor_ptr <- rawPtr rpd
  renderPassEncoderRaw <-
    RawFun.wgpuCommandEncoderBeginRenderPass
      (wgpuHsInstance inst)
      (wgpuCommandEncoder commandEncoder)
      renderPassDescriptor_ptr
  pure (RenderPassEncoder inst renderPassEncoderRaw)

-- | Sets the active render pipeline.
--
-- Subsequent draw calls will exhibit the behaviour defined by the pipeline.
renderPassSetPipeline ::
  MonadIO m =>
  -- | Render pass encoder on which to act.
  RenderPassEncoder ->
  -- | Render pipeline to set active.
  RenderPipeline ->
  -- | IO action which sets the active render pipeline.
  m ()
renderPassSetPipeline renderPassEncoder renderPipeline = do
  let inst = renderPassEncoderInst renderPassEncoder
  RawFun.wgpuRenderPassEncoderSetPipeline
    (wgpuHsInstance inst)
    (wgpuRenderPassEncoder renderPassEncoder)
    (wgpuRenderPipeline renderPipeline)

-- | Draws primitives from the active vertex buffers.
renderPassDraw ::
  MonadIO m =>
  -- | Render pass encoder on which to act.
  RenderPassEncoder ->
  -- | Range of vertices to draw.
  Range Word32 ->
  -- | Range of instances to draw.
  Range Word32 ->
  -- | IO action which stores the draw command.
  m ()
renderPassDraw renderPassEncoder vertices instances = do
  let inst = renderPassEncoderInst renderPassEncoder
  RawFun.wgpuRenderPassEncoderDraw
    (wgpuHsInstance inst)
    (wgpuRenderPassEncoder renderPassEncoder)
    (rangeLength (vertices :: Range Word32))
    (rangeLength (instances :: Range Word32))
    (rangeStart vertices)
    (rangeStart instances)

-- | Finish recording of a render pass.
endRenderPass ::
  MonadIO m =>
  -- | Render pass encoder on which to finish recording.
  RenderPassEncoder ->
  -- | IO action that finishes recording.
  m ()
endRenderPass renderPassEncoder = do
  let inst = renderPassEncoderInst renderPassEncoder
  RawFun.wgpuRenderPassEncoderEndPass
    (wgpuHsInstance inst)
    (wgpuRenderPassEncoder renderPassEncoder)
