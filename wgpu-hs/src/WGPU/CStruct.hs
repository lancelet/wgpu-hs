-- |
module WGPU.CStruct where

import Foreign

class CStruct a where
  sizeOfCStruct :: a -> Int
  alignmentCStruct :: a -> Int
  peekCStruct :: Ptr a -> IO a
  withCStruct :: a -> (Ptr a -> IO b) -> IO b

allocaCStruct :: (CStruct a) => a -> (Ptr a -> IO b) -> IO b
allocaCStruct x = allocaBytesAligned (sizeOfCStruct x) (alignmentCStruct x)
