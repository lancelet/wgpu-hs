{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : WGPU.Internal.Queue
-- Description : Queues
module WGPU.Internal.Queue
  ( -- * Types
    Queue,

    -- * Functions
    getQueue,
    queueSubmit,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Vector (Vector)
import WGPU.Internal.CommandBuffer (CommandBuffer)
import WGPU.Internal.Device (Device, deviceInst, wgpuDevice)
import WGPU.Internal.Instance (Instance, wgpuHsInstance)
import WGPU.Internal.Memory (ToRaw, evalContT, raw, rawArrayPtr, showWithPtr)
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
