{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : WGPU.Internal.CommandBuffer
-- Description : Command buffers.
module WGPU.Internal.CommandBuffer
  ( -- * Types
    CommandBuffer (..),
  )
where

import WGPU.Internal.Memory (ToRaw, raw, showWithPtr)
import WGPU.Raw.Types (WGPUCommandBuffer (WGPUCommandBuffer))

-------------------------------------------------------------------------------

newtype CommandBuffer = CommandBuffer {wgpuCommandBuffer :: WGPUCommandBuffer}

instance Show CommandBuffer where
  show b =
    let CommandBuffer (WGPUCommandBuffer ptr) = b
     in showWithPtr "CommandBuffer" ptr

instance Eq CommandBuffer where
  (==) b1 b2 =
    let CommandBuffer (WGPUCommandBuffer b1_ptr) = b1
        CommandBuffer (WGPUCommandBuffer b2_ptr) = b2
     in b1_ptr == b2_ptr

instance ToRaw CommandBuffer WGPUCommandBuffer where
  raw = pure . wgpuCommandBuffer
