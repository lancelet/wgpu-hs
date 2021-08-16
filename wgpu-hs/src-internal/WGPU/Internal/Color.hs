{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : WGPU.Internal.Color
-- Description : Color.
module WGPU.Internal.Color
  ( -- * Types
    Color (..),

    -- * Functions
    transparentBlack,
  )
where

import Foreign.C (CDouble (CDouble))
import WGPU.Internal.Memory (ToRaw, raw)
import WGPU.Raw.Generated.Struct.WGPUColor (WGPUColor)
import qualified WGPU.Raw.Generated.Struct.WGPUColor as WGPUColor

-- | RGBA double-precision color.
data Color = Color
  { red :: Double,
    green :: Double,
    blue :: Double,
    alpha :: Double
  }
  deriving (Eq, Show)

transparentBlack :: Color
transparentBlack = Color 0 0 0 0

instance ToRaw Color WGPUColor where
  raw Color {..} =
    pure $
      WGPUColor.WGPUColor
        (CDouble red)
        (CDouble green)
        (CDouble blue)
        (CDouble alpha)
