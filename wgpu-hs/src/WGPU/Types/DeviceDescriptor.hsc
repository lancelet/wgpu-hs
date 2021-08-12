{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
-- |

module WGPU.Types.DeviceDescriptor where

#include "wgpu.h"

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (ContT (ContT), evalContT)
import Data.Kind (Type)
import WGPU.CStruct as CStruct
import WGPU.Chain (Chain, HasChainPtr, PeekChain, PokeChain)
import qualified WGPU.Chain as Chain
import Foreign (peekByteOff, plusPtr, pokeByteOff)

data DeviceDescriptor (es :: [Type]) = DeviceDescriptor
  { nextInChain :: !(Chain es)
  }

deriving instance Eq (Chain es) => Eq (DeviceDescriptor es)
deriving instance Show (Chain es) => Show (DeviceDescriptor es)

def :: DeviceDescriptor '[]
def = DeviceDescriptor ()

instance HasChainPtr (DeviceDescriptor es) where
  peekChainPtr ptr = (#peek WGPUDeviceDescriptor, nextInChain) ptr
  pokeChainPtr ptr c_chain = (#poke WGPUDeviceDescriptor, nextInChain) ptr c_chain

instance (PeekChain es, PokeChain es) => CStruct (DeviceDescriptor es) where

  sizeOfCStruct _  = (#size WGPUDeviceDescriptor)
  alignmentCStruct = CStruct.sizeOfCStruct

  peekCStruct ptr = do
    nextInChain <- Chain.peekChain $ (#ptr WGPUDeviceDescriptor, nextInChain) ptr
    pure $! DeviceDescriptor{..}

  withCStruct x@DeviceDescriptor{..} action =
    CStruct.allocaCStruct x $ \ptr ->
      evalContT $ do
        (_, c_chain) <- ContT $ Chain.withChain nextInChain
        lift $ (#poke WGPUDeviceDescriptor, nextInChain) ptr c_chain
        lift $ action ptr
