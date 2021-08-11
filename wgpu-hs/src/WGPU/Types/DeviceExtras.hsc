{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |

module WGPU.Types.DeviceExtras where

#include "wgpu.h"

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (ContT (ContT), evalContT)
import Data.Kind (Type)
import Data.Word (Word32)
import Data.Text (Text)
import qualified Data.Text as Text
import Foreign (peekByteOff, plusPtr,pokeByteOff)
import Foreign.C.String (peekCString, withCString)
import WGPU.CStruct (CStruct)
import qualified WGPU.CStruct as CStruct
import WGPU.Chain (Chain, HasChainPtr, PeekChain, PokeChain)
import qualified WGPU.Chain as Chain
import WGPU.Types.NativeFeature (NativeFeature(NativeFeature))
import qualified WGPU.Types.NativeFeature as NativeFeature

data DeviceExtras (es :: [Type]) = DeviceExtras
  { chain                                     :: !(Chain es),
    maxTextureDimension1D                     :: !Word32,
    maxTextureDimension2D                     :: !Word32,
    maxTextureDimension3D                     :: !Word32,
    maxBindGroups                             :: !Word32,
    maxDynamicStorageBuffersPerPipelineLayout :: !Word32,
    maxStorageBuffersPerShaderStage           :: !Word32,
    maxStorageBufferBindingSize               :: !Word32,
    nativeFeatures                            :: !NativeFeature,
    label                                     :: !Text,
    tracePath                                 :: !Text
  }

deriving instance Eq (Chain es) => Eq (DeviceExtras es)
deriving instance Show (Chain es) => Show (DeviceExtras es)

def :: DeviceExtras '[]
def =
  let
    chain                                     = ()
    maxTextureDimension1D                     = 0
    maxTextureDimension2D                     = 0
    maxTextureDimension3D                     = 0
    maxBindGroups                             = 0
    maxDynamicStorageBuffersPerPipelineLayout = 0
    maxStorageBuffersPerShaderStage           = 0
    maxStorageBufferBindingSize               = 0
    nativeFeatures                            = NativeFeature.NONE
    label                                     = Text.empty
    tracePath                                 = Text.empty
  in DeviceExtras{..}

instance HasChainPtr (DeviceExtras es) where
  peekChainPtr ptr = (#peek WGPUDeviceExtras, chain) ptr
  pokeChainPtr ptr c_chain = (#poke WGPUDeviceExtras, chain) ptr c_chain

instance (PeekChain es, PokeChain es) => CStruct (DeviceExtras es) where

  sizeOfCStruct _ = (#size WGPUDeviceExtras)
  alignmentCStruct = CStruct.sizeOfCStruct

  peekCStruct ptr = do
    chain                                     <- Chain.peekChain $ (#ptr WGPUDeviceExtras, chain) ptr
    maxTextureDimension1D                     <- (#peek WGPUDeviceExtras, maxTextureDimension1D) ptr
    maxTextureDimension2D                     <- (#peek WGPUDeviceExtras, maxTextureDimension2D) ptr
    maxTextureDimension3D                     <- (#peek WGPUDeviceExtras, maxTextureDimension3D) ptr
    maxBindGroups                             <- (#peek WGPUDeviceExtras, maxBindGroups) ptr
    maxDynamicStorageBuffersPerPipelineLayout <- (#peek WGPUDeviceExtras, maxDynamicStorageBuffersPerPipelineLayout) ptr
    maxStorageBuffersPerShaderStage           <- (#peek WGPUDeviceExtras, maxStorageBuffersPerShaderStage) ptr
    maxStorageBufferBindingSize               <- (#peek WGPUDeviceExtras, maxStorageBufferBindingSize) ptr
    c_nativeFeatures                          <- (#peek WGPUDeviceExtras, nativeFeatures) ptr
    c_label                                   <- (#peek WGPUDeviceExtras, label) ptr >>= peekCString
    c_tracePath                               <- (#peek WGPUDeviceExtras, tracePath) ptr >>= peekCString
    let
      nativeFeatures = NativeFeature c_nativeFeatures
      label          = Text.pack c_label
      tracePath      = Text.pack c_tracePath
    pure $! DeviceExtras{..}

  withCStruct x@DeviceExtras{..} action =
    CStruct.allocaCStruct x $ \ptr ->
      evalContT $ do
        let NativeFeature c_nativeFeatures = nativeFeatures
        c_label      <- ContT $ withCString (Text.unpack label)
        c_tracePath  <- ContT $ withCString (Text.unpack tracePath)
        (_, c_chain) <- ContT $ Chain.withChain chain
        lift $ (#poke WGPUDeviceExtras, chain) ptr c_chain
        lift $ (#poke WGPUDeviceExtras, maxTextureDimension1D)                     ptr maxTextureDimension1D
        lift $ (#poke WGPUDeviceExtras, maxTextureDimension2D)                     ptr maxTextureDimension2D
        lift $ (#poke WGPUDeviceExtras, maxTextureDimension3D)                     ptr maxTextureDimension3D
        lift $ (#poke WGPUDeviceExtras, maxBindGroups)                             ptr maxBindGroups
        lift $ (#poke WGPUDeviceExtras, maxDynamicStorageBuffersPerPipelineLayout) ptr maxDynamicStorageBuffersPerPipelineLayout
        lift $ (#poke WGPUDeviceExtras, maxStorageBuffersPerShaderStage)           ptr maxStorageBuffersPerShaderStage
        lift $ (#poke WGPUDeviceExtras, maxStorageBufferBindingSize)               ptr maxStorageBufferBindingSize
        lift $ (#poke WGPUDeviceExtras, nativeFeatures)                            ptr c_nativeFeatures
        lift $ (#poke WGPUDeviceExtras, label)                                     ptr c_label
        lift $ (#poke WGPUDeviceExtras, tracePath)                                 ptr c_tracePath
        lift $ action ptr
