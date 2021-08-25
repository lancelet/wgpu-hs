{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : WGPU.Internal.Surface
-- Description : Platform-specific surfaces.
--
-- Device-specific surfaces.
module WGPU.Internal.Surface
  ( -- * Types
    Surface (..),

    -- * Functions
    createGLFWSurface,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Graphics.UI.GLFW as GLFW
import WGPU.Internal.Instance (Instance, wgpuHsInstance)
import WGPU.Internal.Memory (ToRaw, raw, showWithPtr)
import qualified WGPU.Raw.GLFWSurface
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

-------------------------------------------------------------------------------

-- | Create a WGPU 'Surface' for a GLFW 'GLFW.Window'.
--
-- This function is not part of the @wgpu-native@ API, but is part of the
-- Haskell API until the native WGPU API has a better story around windowing.
createGLFWSurface ::
  MonadIO m =>
  -- | API instance.
  Instance ->
  -- | GLFW window for which the surface will be created.
  GLFW.Window ->
  -- | IO action to create the surface.
  m Surface
createGLFWSurface inst window =
  liftIO $
    Surface inst
      <$> WGPU.Raw.GLFWSurface.createSurface (wgpuHsInstance inst) window
