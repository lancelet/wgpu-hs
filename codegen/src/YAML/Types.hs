{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- {-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module YAML.Types
  ( -- * Types

    -- ** Domain Types
    Name (unName),
    Value64 (..),
    Constant (..),
    EnumEntry (..),
    EnumVariant (..),

    -- ** Errors
    Error (..),

    -- * Smart Constructors
    mkName,

    -- * Quasiquoters
    nameQQ,
  )
where

import Control.Monad ((>=>))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import Data.Yaml
  ( FromJSON (parseJSON),
    Parser,
    ToJSON (toJSON),
    Value (Number, Object, String),
    object,
    withObject,
    (.:),
    (.=),
  )
import GHC.Generics (Generic)
import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (lift)
import Text.Regex.TDFA ((=~))
import Text.Regex.TDFA.Text ()
import TextShow (TextShow (..), showt)
import TextShow.Generic (FromGeneric (..))

---- DOMAIN TYPES ---------------------------------------------------------------------------------

data Value64
  = V64Number !Word64
  | V64UsizeMax
  | V64Uint32Max
  | V64Uint64Max
  | V64Nan
  deriving stock (Eq, Generic)
  deriving (TextShow) via FromGeneric Value64
  deriving (Show) via FromTextShow Value64

newtype Name = Name {unName :: Text}
  deriving stock (Eq, Generic)
  deriving (TextShow) via FromGeneric Name
  deriving (Show) via FromTextShow Name

data Constant = Constant
  { name :: !Name,
    value :: !Value64,
    doc :: !Text
  }
  deriving stock (Eq, Generic)
  deriving (TextShow) via FromGeneric Constant
  deriving (Show) via FromTextShow Constant

data EnumVariant = EnumVariant
  { name :: !Name,
    doc :: !Text
  }
  deriving stock (Eq, Generic)
  deriving (TextShow) via FromGeneric EnumVariant
  deriving (Show) via FromTextShow EnumVariant

data EnumEntry = NullEnumEntry | EnumEntry !EnumVariant
  deriving stock (Eq, Generic)
  deriving (TextShow) via FromGeneric EnumEntry
  deriving (Show) via FromTextShow EnumEntry

---- ERROR TYPE -----------------------------------------------------------------------------------

data Error
  = InvalidName
  { input :: !Text,
    pattern :: !Text
  }
  deriving stock (Eq)
  deriving (Show) via FromTextShow Error

---- SMART CONSTRUCTORS ---------------------------------------------------------------------------

mkName :: Text -> Either Error Name
mkName t
  | t =~ namePattern = Right (Name t)
  | otherwise = Left (InvalidName t namePattern)
  where
    namePattern = "^[a-zA-Z0-9]([a-zA-Z0-9_]*[a-zA-Z0-9])?$"

---- QUASIQUOTERS ---------------------------------------------------------------------------------

-- | Template Haskell function to create a Name at compile time
mkNameTH :: String -> Q Exp
mkNameTH s = case mkName (T.pack s) of
  Right (Name t) -> [|Name $(lift t)|]
  Left err -> error $ "Invalid name at compile time: " <> show err

-- | QuasiQuoter for creating Names with compile-time validation
nameQQ :: QuasiQuoter
nameQQ =
  QuasiQuoter
    { quoteExp = mkNameTH,
      quotePat = error "nameQQ QuasiQuoter: patterns not supported",
      quoteType = error "nameQQ QuasiQuoter: types not supported",
      quoteDec = error "nameQQ QuasiQuoter: declarations not supported"
    }

---- TEXTSHOW INSTANCES ---------------------------------------------------------------------------

instance TextShow Error where
  showb (InvalidName input pattern) =
    "Invalid name '" <> showb input <> "': must match pattern " <> showb pattern

---- JSON / YAML INSTANCES ------------------------------------------------------------------------

instance ToJSON Value64 where
  toJSON (V64Number n) = Number (fromIntegral n)
  toJSON V64UsizeMax = String "usize_max"
  toJSON V64Uint32Max = String "uint32_max"
  toJSON V64Uint64Max = String "uint64_max"
  toJSON V64Nan = String "nan"

instance FromJSON Value64 where
  parseJSON (Number n) = pure $ V64Number (round n)
  parseJSON (String s) = case s of
    "usize_max" -> pure V64UsizeMax
    "uint32_max" -> pure V64Uint32Max
    "uint64_max" -> pure V64Uint64Max
    "nan" -> pure V64Nan
    _ -> failText $ "Invalid Value64: " <> s
  parseJSON _ = failText "Value64 must be a number or string"

instance ToJSON Name where
  toJSON (Name t) = toJSON t

instance FromJSON Name where
  parseJSON = parseWith mkName

instance ToJSON Constant where
  toJSON (Constant n v d) =
    object
      [ "name" .= n,
        "value" .= v,
        "doc" .= d
      ]

instance FromJSON Constant where
  parseJSON = withObject "Constant" $ \o ->
    Constant
      <$> o .: "name"
      <*> o .: "value"
      <*> o .: "doc"

instance ToJSON EnumVariant where
  toJSON (EnumVariant n d) =
    object
      [ "name" .= n,
        "doc" .= d
      ]

instance FromJSON EnumVariant where
  parseJSON = withObject "NamedEnumEntry" $ \o ->
    EnumVariant
      <$> o .: "name"
      <*> o .: "doc"

instance ToJSON EnumEntry where
  toJSON NullEnumEntry = String "null"
  toJSON (EnumEntry v) = toJSON v

instance FromJSON EnumEntry where
  parseJSON (String "null") = pure NullEnumEntry
  parseJSON (Object o) = EnumEntry <$> parseJSON (Object o)
  parseJSON _ = failText "EnumEntry must be a string or object"

---- HELPER FUNCTIONS -----------------------------------------------------------------------------

-- | Converts a text creation function to a JSON parser
parseWith :: (Text -> Either Error a) -> Value -> Parser a
parseWith f = parseJSON >=> either failShow pure . f

-- | Text-friendly version of fail
failText :: (MonadFail m) => Text -> m a
failText = fail . T.unpack

-- | Fail with any TextShow-able value
failShow :: (MonadFail m, TextShow a) => a -> m b
failShow = failText . showt

---- DERIVING VIA WRAPPER -------------------------------------------------------------------------

newtype FromTextShow a = FromTextShow a

instance (TextShow a) => Show (FromTextShow a) where
  show (FromTextShow x) = T.unpack (showt x)
