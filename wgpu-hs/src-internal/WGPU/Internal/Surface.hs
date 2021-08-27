{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : WGPU.Internal.Surface
-- Description : Platform-specific surfaces.
module WGPU.Internal.Surface
  ( -- * Types
    Surface (..),
  )
where

import WGPU.Internal.Instance (Instance)
import WGPU.Internal.Memory (ToRaw, raw, showWithPtr)
import WGPU.Raw.Types (WGPUSurface (WGPUSurface))

-------------------------------------------------------------------------------

-- | Handle to a presentable surface.
--
-- A 'Surface' presents a platform-specific surface (eg. a window) on to which
-- rendered images may be presented. A 'Surface' can be created for a GLFW
-- window using 'createGLFWSurface'.
data Surface = Surface
  { surfaceInst :: !Instance,
    wgpuSurface :: !WGPUSurface
  }

instance Show Surface where
  show s =
    let Surface _ (WGPUSurface ptr) = s
     in showWithPtr "Surface" ptr

instance Eq Surface where
  (==) s1 s2 =
    let Surface _ (WGPUSurface s1_ptr) = s1
        Surface _ (WGPUSurface s2_ptr) = s2
     in s1_ptr == s2_ptr

instance ToRaw Surface WGPUSurface where
  raw = pure . wgpuSurface
