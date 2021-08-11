{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
-- |

module WGPU.Types.SType (SType(..)) where

#include "wgpu.h"

import Data.Word (Word32)
import Foreign.C.Enum (enum)

enum "SType"
  ''Word32
  [''Show, ''Read, ''Eq]
  [ ("Invalid",
     #{const WGPUSType_Invalid}),
    ("SurfaceDescriptorFromMetalLayer",
     #{const WGPUSType_SurfaceDescriptorFromMetalLayer}),
    ("SurfaceDescriptorFromWindowsHWND",
     #{const WGPUSType_SurfaceDescriptorFromWindowsHWND}),
    ("SurfaceDescriptorFromXlib",
     #{const WGPUSType_SurfaceDescriptorFromXlib}),
    ("SurfaceDescriptorFromCanvasHTMLSelector",
     #{const WGPUSType_SurfaceDescriptorFromCanvasHTMLSelector}),
    ("ShaderModuleSPIRVDescriptor",
     #{const WGPUSType_ShaderModuleSPIRVDescriptor}),
    ("ShaderModuleWGSLDescriptor",
     #{const WGPUSType_ShaderModuleWGSLDescriptor}),
    ("PrimitiveDepthClampingState",
     #{const WGPUSType_PrimitiveDepthClampingState}),
    ("Force32",
     #{const WGPUSType_Force32})
  ]
