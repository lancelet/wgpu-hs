{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
-- |
module WGPU.Types.NativeFeature (NativeFeature(..)) where

#include "wgpu.h"

import Data.Word (Word32)
import Foreign.C.Enum (enum)

enum "NativeFeature"
  ''Word32
  [''Show, ''Read, ''Eq]
  [ ("TEXTURE_ADAPTER_SPECIFIC_FORMAT_FEATURES",
     #{const WGPUNativeFeature_TEXTURE_ADAPTER_SPECIFIC_FORMAT_FEATURES})
  ]
