{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : WGPU.Internal.Multipurpose
-- Description : Multipurpose
--
-- This is a bit like a "Types" module. It exists to collect things which are
-- somewhat generic and are used in more than one part of the API.
module WGPU.Internal.Multipurpose
  ( -- * Types
    CompareFunction (..),
  )
where

import WGPU.Internal.Memory (ToRaw, raw)
import WGPU.Raw.Generated.Enum.WGPUCompareFunction (WGPUCompareFunction)
import qualified WGPU.Raw.Generated.Enum.WGPUCompareFunction as WGPUCompareFunction

-------------------------------------------------------------------------------

-- | Comparison function used for depth and stencil operations.
data CompareFunction
  = CompareFunctionNever
  | CompareFunctionLess
  | CompareFunctionEqual
  | CompareFunctionLessEqual
  | CompareFunctionGreater
  | CompareFunctionNotEqual
  | CompareFunctionGreaterEqual
  | CompareFunctionAlways
  deriving (Eq, Show)

-- | Convert a 'CompareFunction' to its raw value.
instance ToRaw CompareFunction WGPUCompareFunction where
  raw cf =
    pure $
      case cf of
        CompareFunctionNever -> WGPUCompareFunction.Never
        CompareFunctionLess -> WGPUCompareFunction.Less
        CompareFunctionEqual -> WGPUCompareFunction.Equal
        CompareFunctionLessEqual -> WGPUCompareFunction.LessEqual
        CompareFunctionGreater -> WGPUCompareFunction.Greater
        CompareFunctionNotEqual -> WGPUCompareFunction.NotEqual
        CompareFunctionGreaterEqual -> WGPUCompareFunction.GreaterEqual
        CompareFunctionAlways -> WGPUCompareFunction.Always
