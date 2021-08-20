{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module WGPU.Internal.ChainedStruct
  ( -- * Types
    ChainedStruct (..),
  )
where

import Foreign (Ptr, castPtr, nullPtr)
import WGPU.Internal.Memory (ToRaw, raw)
import WGPU.Raw.Generated.Enum.WGPUSType (WGPUSType)
import WGPU.Raw.Generated.Struct.WGPUChainedStruct (WGPUChainedStruct)
import qualified WGPU.Raw.Generated.Struct.WGPUChainedStruct as WGPUChainedStruct

-- | Represents a chained structure.
data ChainedStruct a
  = -- | Empty chained structure, but with a type tag.
    EmptyChain WGPUSType
  | -- | Chained structue that points to the next structure in a chain.
    PtrChain WGPUSType (Ptr a)

instance ToRaw (ChainedStruct a) WGPUChainedStruct where
  raw chainedStruct =
    case chainedStruct of
      EmptyChain sType ->
        pure
          WGPUChainedStruct.WGPUChainedStruct
            { next = nullPtr,
              sType = sType
            }
      PtrChain sType ptr ->
        pure
          WGPUChainedStruct.WGPUChainedStruct
            { next = castPtr ptr,
              sType = sType
            }
