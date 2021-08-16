-- |
-- Module      : WGPU.Internal.SMaybe
-- Description : Strict maybe.
module WGPU.Internal.SMaybe
  ( -- * Types
    SMaybe (SNothing, SJust),

    -- * Functions
    fromSMaybe,
  )
where

-- | Strict version of the 'Maybe' type.
data SMaybe a
  = SNothing
  | SJust !a
  deriving (Eq, Show)

-- | Return a value from an 'SMaybe' with a default.
--
-- This function returns the 'SJust' value from an 'SMaybe', or the default
-- value if the 'SMaybe' is 'SNothing'.
fromSMaybe ::
  -- | Default value.
  a ->
  -- | 'SMaybe' from which to return the 'SJust' value if possible.
  SMaybe a ->
  -- | 'SJust' value, if present, or the default value, if not.
  a
fromSMaybe defaultValue sMaybe =
  case sMaybe of
    SNothing -> defaultValue
    SJust providedValue -> providedValue
