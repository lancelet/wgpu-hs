-- |
-- Module      : WGPU.Internal.SDL.Surface
-- Description : SDL-specific surfaces.
module WGPU.Internal.SDL.Surface
  ( -- * Functions
    createSurface,
  )
where

import Control.Monad.IO.Class (MonadIO)
import qualified SDL (Window)
import WGPU.Internal.Instance (Instance, wgpuHsInstance)
import WGPU.Internal.Surface (Surface (Surface))
import qualified WGPU.Raw.SDLSurface (createSurface)

createSurface ::
  MonadIO m =>
  Instance ->
  SDL.Window ->
  m Surface
createSurface inst window =
  Surface inst
    <$> WGPU.Raw.SDLSurface.createSurface (wgpuHsInstance inst) window
