-- |
module WGPU.CStruct where

import Foreign (Ptr, allocaBytesAligned)

-- NOTE: The point of CStruct as opposed to Storable is that it doesn't have
--       a `poke`. Instead, it has a `withCStruct`, which allows for cleanup
--       of C strings, etc.
class CStruct a where
  sizeOfCStruct :: a -> Int
  alignmentCStruct :: a -> Int
  peekCStruct :: Ptr a -> IO a
  withCStruct :: a -> (Ptr a -> IO b) -> IO b

allocaCStruct :: (CStruct a) => a -> (Ptr a -> IO b) -> IO b
allocaCStruct x action = do
  allocaBytesAligned
    (sizeOfCStruct x)
    (alignmentCStruct x)
    action
