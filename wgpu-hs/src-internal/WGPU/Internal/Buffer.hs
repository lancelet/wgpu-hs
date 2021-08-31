{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WGPU.Internal.Buffer
  ( -- * Types
    Buffer (..),
    BufferUsage (..),
    BufferDescriptor (..),

    -- * Functions
    createBuffer,
    createBufferInit,
  )
where

import Control.Monad.Cont (ContT (ContT))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits ((.|.))
import Data.Default (Default, def)
import Data.Text (Text)
import Data.Word (Word32, Word64)
import Foreign
  ( Ptr,
    castPtr,
    copyBytes,
    nullPtr,
  )
import WGPU.Internal.Device (Device, deviceInst, wgpuDevice)
import WGPU.Internal.Instance (Instance, wgpuHsInstance)
import WGPU.Internal.Memory
  ( ByteSize,
    ReadableMemoryBuffer,
    ToRaw,
    evalContT,
    raw,
    rawPtr,
    readableMemoryBufferSize,
    showWithPtr,
    toCSize,
    unByteSize,
    withReadablePtr,
  )
import qualified WGPU.Raw.Generated.Enum.WGPUBufferUsage as WGPUBufferUsage
import qualified WGPU.Raw.Generated.Fun as RawFun
import qualified WGPU.Raw.Generated.Fun as WGPU
import WGPU.Raw.Generated.Struct.WGPUBufferDescriptor (WGPUBufferDescriptor)
import qualified WGPU.Raw.Generated.Struct.WGPUBufferDescriptor as WGPUBufferDescriptor
import WGPU.Raw.Types (WGPUBuffer (WGPUBuffer))

-------------------------------------------------------------------------------

-- | Handle to a buffer.
data Buffer = Buffer
  { bufferInst :: !Instance,
    bufferDevice :: !Device,
    wgpuBuffer :: !WGPUBuffer
  }

instance Show Buffer where
  show b =
    let Buffer _ _ (WGPUBuffer ptr) = b
     in showWithPtr "Buffer" ptr

instance Eq Buffer where
  (==) b1 b2 =
    let Buffer _ _ (WGPUBuffer b1_ptr) = b1
        Buffer _ _ (WGPUBuffer b2_ptr) = b2
     in b1_ptr == b2_ptr

instance ToRaw Buffer WGPUBuffer where
  raw = pure . wgpuBuffer

-------------------------------------------------------------------------------

-- | Different ways you can use a buffer.
data BufferUsage = BufferUsage
  { -- | Allow a buffer to be mapped for reading.
    bufMapRead :: !Bool,
    -- | Allow a buffer to be mapped for writing.
    bufMapWrite :: !Bool,
    -- | Allow a buffer to be a source buffer for a copy operation.
    bufCopySrc :: !Bool,
    -- | Allow a buffer to be a destination buffer for a copy operation.
    bufCopyDst :: !Bool,
    -- | Allow a buffer to be the index buffer in a draw operation.
    bufIndex :: !Bool,
    -- | Allow a buffer to be the vertex buffer in a draw operation.
    bufVertex :: !Bool,
    -- | Allow a buffer to be a uniform binding in a bind group.
    bufUniform :: !Bool,
    -- | Allow a buffer to be a storage binding in a bind group.
    bufStorage :: !Bool,
    -- | Allow a buffer to be the indirect buffer in an indirect draw call.
    bufIndirect :: !Bool
  }
  deriving (Eq, Show)

instance ToRaw BufferUsage Word32 where
  raw BufferUsage {..} =
    pure $
      (if bufMapRead then WGPUBufferUsage.MapRead else 0)
        .|. (if bufMapWrite then WGPUBufferUsage.MapWrite else 0)
        .|. (if bufCopySrc then WGPUBufferUsage.CopySrc else 0)
        .|. (if bufCopyDst then WGPUBufferUsage.CopyDst else 0)
        .|. (if bufIndex then WGPUBufferUsage.Index else 0)
        .|. (if bufVertex then WGPUBufferUsage.Vertex else 0)
        .|. (if bufUniform then WGPUBufferUsage.Uniform else 0)
        .|. (if bufStorage then WGPUBufferUsage.Storage else 0)
        .|. (if bufIndirect then WGPUBufferUsage.Indirect else 0)

instance Default BufferUsage where
  def =
    BufferUsage
      { bufMapRead = False,
        bufMapWrite = False,
        bufCopySrc = False,
        bufCopyDst = False,
        bufIndex = False,
        bufVertex = False,
        bufUniform = False,
        bufStorage = False,
        bufIndirect = False
      }

-------------------------------------------------------------------------------

-- | Describes a 'Buffer'.
data BufferDescriptor = BufferDescriptor
  { -- | Debugging label for the buffer.
    bufferLabel :: !Text,
    -- | Size of the buffer, in bytes.
    bufferSize :: !ByteSize,
    -- | Usage(s) of the buffer.
    bufferUsage :: !BufferUsage,
    -- | Is the buffer mapped to host memory at creation? If this is set to
    -- 'True', then the buffer may be more easily populated with data
    -- initially. See 'createBufferInit' for a way to create a buffer and
    -- initialize it with data in one step.
    mappedAtCreation :: Bool
  }
  deriving (Eq, Show)

instance ToRaw BufferDescriptor WGPUBufferDescriptor where
  raw BufferDescriptor {..} = do
    label_ptr <- rawPtr bufferLabel
    n_usage <- raw bufferUsage
    n_mappedAtCreation <- raw mappedAtCreation
    pure
      WGPUBufferDescriptor.WGPUBufferDescriptor
        { nextInChain = nullPtr,
          label = label_ptr,
          usage = n_usage,
          size = unByteSize bufferSize,
          mappedAtCreation = n_mappedAtCreation
        }

-------------------------------------------------------------------------------

-- | Create a 'Buffer'.
createBuffer :: MonadIO m => Device -> BufferDescriptor -> m Buffer
createBuffer device bufferDescriptor = liftIO . evalContT $ do
  let inst = deviceInst device
  bufferDescriptor_ptr <- rawPtr bufferDescriptor
  Buffer inst device
    <$> RawFun.wgpuDeviceCreateBuffer
      (wgpuHsInstance inst)
      (wgpuDevice device)
      bufferDescriptor_ptr

-- | Create a 'Buffer' with data to initialize it.
createBufferInit ::
  forall a m.
  (MonadIO m, ReadableMemoryBuffer a) =>
  -- | Device for which to create the buffer.
  Device ->
  -- | Debugging label for the buffer.
  Text ->
  -- | Usage for the buffer.
  BufferUsage ->
  -- | Data to initialize the buffer with.
  a ->
  -- | Buffer created with the specified data.
  m Buffer
createBufferInit device label bufferUsage content = liftIO . evalContT $ do
  -- Convert the foreign pointer to a raw pointer.
  contentPtr <- ContT $ withReadablePtr content
  let contentSz :: ByteSize
      contentSz = readableMemoryBufferSize content

  -- Create the buffer, marking it as "mappedAtCreation", so that its memory
  -- is mapped to host memory.
  let bufferDescriptor :: BufferDescriptor
      bufferDescriptor =
        BufferDescriptor
          { bufferLabel = label,
            bufferSize = contentSz,
            bufferUsage = bufferUsage,
            mappedAtCreation = True
          }
  buffer <- createBuffer device bufferDescriptor

  -- Find the pointer to the mapped region of the buffer.
  bufferPtr <- bufferGetMappedRange buffer 0 contentSz

  -- Copy the supplied content to the buffer
  liftIO $ copyBytes bufferPtr (castPtr contentPtr) (fromIntegral contentSz)

  -- Un-map the buffer and return it
  bufferUnmap buffer
  pure buffer

-- | Return a pointer to a region of host memory that has been mapped to a
-- buffer.
bufferGetMappedRange :: MonadIO m => Buffer -> Word64 -> ByteSize -> m (Ptr ())
bufferGetMappedRange buffer byteOffset byteLength = do
  let inst = bufferInst buffer
  WGPU.wgpuBufferGetMappedRange
    (wgpuHsInstance inst)
    (wgpuBuffer buffer)
    (fromIntegral byteOffset)
    (toCSize byteLength)
{-# INLINEABLE bufferGetMappedRange #-}

-- | Unmap a buffer that was previously mapped into host memory.
bufferUnmap :: MonadIO m => Buffer -> m ()
bufferUnmap buffer = do
  let inst = bufferInst buffer
  WGPU.wgpuBufferUnmap
    (wgpuHsInstance inst)
    (wgpuBuffer buffer)
{-# INLINEABLE bufferUnmap #-}
