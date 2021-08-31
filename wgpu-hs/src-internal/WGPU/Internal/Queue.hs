{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : WGPU.Internal.Queue
-- Description : Queues
module WGPU.Internal.Queue
  ( -- * Types
    Queue,

    -- * Functions
    getQueue,
    queueSubmit,
    queueWriteTexture,
    queueWriteBuffer,
  )
where

import Control.Monad.Cont (ContT (ContT))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Vector (Vector)
import Foreign (castPtr)
import WGPU.Internal.Buffer (Buffer, wgpuBuffer)
import WGPU.Internal.CommandBuffer (CommandBuffer)
import WGPU.Internal.Device (Device, deviceInst, wgpuDevice)
import WGPU.Internal.Instance (Instance, wgpuHsInstance)
import WGPU.Internal.Memory
  ( ReadableMemoryBuffer,
    ToRaw,
    evalContT,
    raw,
    rawArrayPtr,
    rawPtr,
    readableMemoryBufferSize,
    showWithPtr,
    toCSize,
    withReadablePtr,
  )
import WGPU.Internal.Multipurpose
  ( Extent3D,
    ImageCopyTexture,
    TextureDataLayout,
  )
import qualified WGPU.Raw.Generated.Fun as RawFun
import WGPU.Raw.Types (WGPUQueue (WGPUQueue))

-------------------------------------------------------------------------------

data Queue = Queue
  { queueInst :: !Instance,
    wgpuQueue :: !WGPUQueue
  }

instance Show Queue where
  show q =
    let Queue _ (WGPUQueue ptr) = q
     in showWithPtr "Queue" ptr

instance Eq Queue where
  (==) q1 q2 =
    let Queue _ (WGPUQueue q1_ptr) = q1
        Queue _ (WGPUQueue q2_ptr) = q2
     in q1_ptr == q2_ptr

instance ToRaw Queue WGPUQueue where
  raw = pure . wgpuQueue

-------------------------------------------------------------------------------

-- | Get the queue for a device.
getQueue :: MonadIO m => Device -> m Queue
getQueue device = do
  let queueInst = deviceInst device
  wgpuQueue <-
    RawFun.wgpuDeviceGetQueue (wgpuHsInstance queueInst) (wgpuDevice device)
  pure Queue {..}

-- | Submit a list of command buffers to a device queue.
queueSubmit :: MonadIO m => Queue -> Vector CommandBuffer -> m ()
queueSubmit queue cbs = liftIO . evalContT $ do
  let inst = queueInst queue
  let commandCount = fromIntegral . length $ cbs
  commandBuffer_ptr <- rawArrayPtr cbs
  RawFun.wgpuQueueSubmit
    (wgpuHsInstance inst)
    (wgpuQueue queue)
    commandCount
    commandBuffer_ptr

-------------------------------------------------------------------------------

-- | Schedule a data write into a texture.
queueWriteTexture ::
  forall m a.
  (MonadIO m, ReadableMemoryBuffer a) =>
  -- | Queue to which the texture write will be submitted.
  Queue ->
  -- | View of a texture which will be copied.
  ImageCopyTexture ->
  -- | Layout of the texture in a buffer's memory.
  TextureDataLayout ->
  -- | Extent of the texture operation.
  Extent3D ->
  -- | A 'ReadableMemoryBuffer' from which to copy. All of the buffer is copied
  -- (as determined by its 'readableMemoryBufferSize').
  a ->
  -- | Action to copy the texture
  m ()
queueWriteTexture queue imageCopyTexture textureDataLayout extent3d content =
  liftIO . evalContT $ do
    let inst = queueInst queue
    let content_sz = readableMemoryBufferSize content
    content_ptr <- ContT $ withReadablePtr content
    imageCopyTexture_ptr <- rawPtr imageCopyTexture
    textureDataLayout_ptr <- rawPtr textureDataLayout
    extent3d_ptr <- rawPtr extent3d
    RawFun.wgpuQueueWriteTexture
      (wgpuHsInstance inst)
      (wgpuQueue queue)
      imageCopyTexture_ptr
      content_ptr
      (toCSize content_sz)
      textureDataLayout_ptr
      extent3d_ptr

-- | Schedule a data write into a buffer.
queueWriteBuffer ::
  forall m a.
  (MonadIO m, ReadableMemoryBuffer a) =>
  -- | Queue to which the buffer write will be submitted.
  Queue ->
  -- | Buffer in which to write.
  Buffer ->
  -- | A 'ReadableMemoryBuffer' from which to copy. All of the buffer is copied
  -- (as determined by its 'readableMemoryBufferSize').
  a ->
  -- | Action which copies the buffer data.
  m ()
queueWriteBuffer queue buffer content = liftIO . evalContT $ do
  let inst = queueInst queue
  let content_sz = readableMemoryBufferSize content
  content_ptr <- ContT $ withReadablePtr content
  RawFun.wgpuQueueWriteBuffer
    (wgpuHsInstance inst)
    (wgpuQueue queue)
    (wgpuBuffer buffer)
    0
    (castPtr content_ptr)
    (toCSize content_sz)
