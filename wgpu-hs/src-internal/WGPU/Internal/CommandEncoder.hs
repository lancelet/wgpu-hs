{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : WGPU.Internal.CommandEncoder
-- Description : Command encoding.
module WGPU.Internal.CommandEncoder
  ( -- * Types
    CommandEncoder (..),

    -- * Functions
    createCommandEncoder,
    commandEncoderFinish,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Cont (ContT (ContT), evalContT)
import Data.Text (Text)
import Foreign (nullPtr, with)
import WGPU.Internal.CommandBuffer (CommandBuffer (CommandBuffer))
import WGPU.Internal.Device (Device, deviceInst, wgpuDevice)
import WGPU.Internal.Instance (Instance, wgpuHsInstance)
import WGPU.Internal.Memory (ToRaw, raw, rawPtr, showWithPtr)
import qualified WGPU.Raw.Generated.Fun as RawFun
import WGPU.Raw.Generated.Struct.WGPUCommandBufferDescriptor (WGPUCommandBufferDescriptor)
import qualified WGPU.Raw.Generated.Struct.WGPUCommandBufferDescriptor as WGPUCommandBufferDescriptor
import qualified WGPU.Raw.Generated.Struct.WGPUCommandEncoderDescriptor as WGPUCommandEncoderDescriptor
import WGPU.Raw.Types (WGPUCommandEncoder (WGPUCommandEncoder))

-------------------------------------------------------------------------------

-- | Handle to an encoder for a series of GPU operations.
--
-- A command encoder can record render passes, compute passes, and transfer
-- operations between driver-managed resources like buffers and textures.
data CommandEncoder = CommandEncoder
  { commandEncoderInst :: !Instance,
    wgpuCommandEncoder :: !WGPUCommandEncoder
  }

instance Show CommandEncoder where
  show e =
    let CommandEncoder _ (WGPUCommandEncoder ptr) = e
     in showWithPtr "CommandEncoder" ptr

instance Eq CommandEncoder where
  (==) e1 e2 =
    let CommandEncoder _ (WGPUCommandEncoder e1_ptr) = e1
        CommandEncoder _ (WGPUCommandEncoder e2_ptr) = e2
     in e1_ptr == e2_ptr

instance ToRaw CommandEncoder WGPUCommandEncoder where
  raw = pure . wgpuCommandEncoder

-------------------------------------------------------------------------------

-- | Create an empty command encoder.
createCommandEncoder ::
  -- | Device for which to create the command encoder.
  Device ->
  -- | Debug label for the command encoder.
  Text ->
  -- | IO action that returns the command encoder.
  IO CommandEncoder
createCommandEncoder device label = evalContT $ do
  let inst = deviceInst device
  label_ptr <- rawPtr label
  commandEncoderDescriptor_ptr <-
    ContT . with $
      WGPUCommandEncoderDescriptor.WGPUCommandEncoderDescriptor
        { nextInChain = nullPtr,
          label = label_ptr
        }
  commandEncoderRaw <-
    liftIO $
      RawFun.wgpuDeviceCreateCommandEncoder
        (wgpuHsInstance inst)
        (wgpuDevice device)
        commandEncoderDescriptor_ptr
  pure (CommandEncoder inst commandEncoderRaw)

-- | Finish encoding commands, returning a command buffer.
commandEncoderFinish ::
  -- | Command encoder to finish.
  CommandEncoder ->
  -- | Debugging label for the command buffer.
  Text ->
  -- | IO action which returns the command buffer.
  IO CommandBuffer
commandEncoderFinish commandEncoder label = evalContT $ do
  let inst = commandEncoderInst commandEncoder
  commandBufferDescriptor_ptr <- rawPtr $ CommandBufferDescriptor label
  commandBufferRaw <-
    liftIO $
      RawFun.wgpuCommandEncoderFinish
        (wgpuHsInstance inst)
        (wgpuCommandEncoder commandEncoder)
        commandBufferDescriptor_ptr
  pure (CommandBuffer commandBufferRaw)

-------------------------------------------------------------------------------

newtype CommandBufferDescriptor = CommandBufferDescriptor
  {commandBufferLabel :: Text}
  deriving (Eq, Show)

instance ToRaw CommandBufferDescriptor WGPUCommandBufferDescriptor where
  raw CommandBufferDescriptor {..} = do
    label_ptr <- rawPtr commandBufferLabel
    pure
      WGPUCommandBufferDescriptor.WGPUCommandBufferDescriptor
        { nextInChain = nullPtr,
          label = label_ptr
        }
