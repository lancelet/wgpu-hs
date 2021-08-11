{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
-- |

module WGPU.Types.ChainedStruct
  ( ChainedStruct(..),
    STypeId(..)
  ) where

#include "wgpu.h"

import Foreign

newtype STypeId = STypeId Word32
  deriving (Eq, Show, Num, Storable)

data ChainedStruct = ChainedStruct
  {
    next  :: !(Ptr ()),
    sType :: !STypeId
  }
  deriving (Eq, Show)

instance Storable ChainedStruct where
  sizeOf _ = (#size WGPUChainedStruct)
  alignment = sizeOf
  peek ptr = do
    next  <- (#peek WGPUChainedStruct, next) ptr
    sType <- (#peek WGPUChainedStruct, sType) ptr
    pure $! ChainedStruct{..}
  poke ptr ChainedStruct{..} = do
    #{poke WGPUChainedStruct, next} ptr next
    #{poke WGPUChainedStruct, sType} ptr sType
