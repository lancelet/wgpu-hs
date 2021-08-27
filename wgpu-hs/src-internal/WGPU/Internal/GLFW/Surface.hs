-- |
-- Module      : WGPU.Internal.GLFW.Surface
-- Description : GLFW-specific surfaces.
module WGPU.Internal.GLFW.Surface
  ( -- * Functions
    createSurface,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Graphics.UI.GLFW as GLFW
import WGPU.Internal.Instance (Instance, wgpuHsInstance)
import WGPU.Internal.Surface (Surface (Surface))
import qualified WGPU.Raw.GLFWSurface

-------------------------------------------------------------------------------

-- | Create a WGPU 'Surface' for a GLFW 'GLFW.Window'.
--
-- This function is not part of the @wgpu-native@ API, but is part of the
-- Haskell API until the native WGPU API has a better story around windowing.
createSurface ::
  MonadIO m =>
  -- | API instance.
  Instance ->
  -- | GLFW window for which the surface will be created.
  GLFW.Window ->
  -- | IO action to create the surface.
  m Surface
createSurface inst window =
  liftIO $
    Surface inst
      <$> WGPU.Raw.GLFWSurface.createSurface (wgpuHsInstance inst) window
