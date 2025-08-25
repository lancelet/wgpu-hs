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
    EnumVariant (..),
    EnumEntry (..),
    Enum (..),
    BitFlagEntry (..),
    BitFlag (..),
    StringHint (..),
    F32Nullable (..),
    F64Supertype (..),
    BaseType (..),
    ArrayType (..),
    PrimitiveType (..),
    Pointer (..),
    ComplexClass (..),
    ComplexType (..),

    -- ** Errors
    Error (..),

    -- * Smart Constructors
    mkName,

    -- * Quasiquoters
    nameQQ,
  )
where

import Control.Monad (unless, (>=>))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (keys)
import Data.List (sort)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word64)
import Data.Yaml
  ( FromJSON (parseJSON),
    Object,
    Parser,
    ToJSON (toJSON),
    Value (Null, Number, Object, String),
    object,
    withObject,
    (.!=),
    (.:),
    (.:?),
    (.=),
  )
import GHC.Generics (Generic)
import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (lift)
import Text.Regex.TDFA ((=~))
import Text.Regex.TDFA.Text ()
import TextShow (TextShow (..), showt)
import TextShow.Data.Vector ()
import TextShow.Generic (FromGeneric (..))
import Prelude hiding (Enum)

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

data Enum = Enum
  { name :: !Name,
    doc :: !Text,
    entries :: !(Vector EnumEntry)
  }
  deriving stock (Eq, Generic)
  deriving (TextShow) via FromGeneric Enum
  deriving (Show) via FromTextShow Enum

data BitFlagEntry = BitFlagEntry
  { name :: !Name,
    doc :: !Text,
    value :: !(Maybe Value64),
    valueCombination :: !(Vector Name)
  }
  deriving stock (Eq, Generic)
  deriving (TextShow) via FromGeneric BitFlagEntry
  deriving (Show) via FromTextShow BitFlagEntry

data BitFlag = BitFlag
  { name :: !Name,
    doc :: !Text,
    entries :: !(Vector BitFlagEntry)
  }
  deriving stock (Eq, Generic)
  deriving (TextShow) via FromGeneric BitFlag
  deriving (Show) via FromTextShow BitFlag

data F32Nullable = F32Nullable
  deriving stock (Eq, Generic)
  deriving (TextShow) via FromGeneric F32Nullable
  deriving (Show) via FromTextShow F32Nullable

data F64Supertype = F64Supertype
  deriving stock (Eq, Generic)
  deriving (TextShow) via FromGeneric F64Supertype
  deriving (Show) via FromTextShow F64Supertype

data StringHint = StringNullable | StringWithDefaultEmpty | OutString
  deriving stock (Eq, Generic)
  deriving (TextShow) via FromGeneric StringHint
  deriving (Show) via FromTextShow StringHint

data BaseType
  = Tbool
  | Tstring !(Maybe StringHint)
  | Tuint16
  | Tuint32
  | Tuint64
  | Tusize
  | Tint16
  | Tint32
  | Tfloat32 !(Maybe F32Nullable)
  | Tfloat64 !(Maybe F64Supertype)
  deriving stock (Eq, Generic)
  deriving (TextShow) via FromGeneric BaseType
  deriving (Show) via FromTextShow BaseType

newtype ArrayType = ArrayType {arrayElemType :: BaseType}
  deriving stock (Eq, Generic)
  deriving (TextShow) via FromGeneric ArrayType
  deriving (Show) via FromTextShow ArrayType

data PrimitiveType
  = PTVoid
  | PTBase !BaseType
  | PTArray !ArrayType
  deriving stock (Eq, Generic)
  deriving (TextShow) via FromGeneric PrimitiveType
  deriving (Show) via FromTextShow PrimitiveType

data Pointer = PtrImmutable | PtrMutable
  deriving stock (Eq, Generic)
  deriving (TextShow) via FromGeneric Pointer
  deriving (Show) via FromTextShow Pointer

data ComplexClass
  = CTypedef
  | CEnum
  | CBitFlag
  | CStruct
  | CFunction
  | CObject
  deriving stock (Eq, Generic)
  deriving (TextShow) via FromGeneric ComplexClass
  deriving (Show) via FromTextShow ComplexClass

data ComplexType
  = ComplexType
  { isArray :: !Bool,
    clazz :: !ComplexClass,
    name :: !Name
  }
  deriving stock (Eq, Generic)
  deriving (TextShow) via FromGeneric ComplexType
  deriving (Show) via FromTextShow ComplexType

-- TODO
-- [x] - Bitflags
-- [ ] - Callbacks
-- [ ] - Structs
-- [ ] - Functions
-- [ ] - Objects

---- ERROR TYPE -----------------------------------------------------------------------------------

data Error
  = InvalidName
      { input :: !Text,
        pattern :: !Text
      }
  | InvalidComplexClass {input :: !Text}
  | InvalidComplexTypeNoPeriod {input :: !Text}
  deriving stock (Eq)
  deriving (Show) via FromTextShow Error

---- SMART CONSTRUCTORS / PARSERS -----------------------------------------------------------------

mkName :: Text -> Either Error Name
mkName t
  | t =~ namePattern = Right (Name t)
  | otherwise = Left (InvalidName t namePattern)
  where
    namePattern = "^[a-zA-Z0-9]([a-zA-Z0-9_]*[a-zA-Z0-9])?$"

mkComplexType :: Text -> Either Error ComplexType
mkComplexType txt =
  let p = T.strip txt
      (isArray, inner) =
        if T.isPrefixOf "array<" p && T.isSuffixOf ">" p
          then (True, T.init $ T.drop 6 p)
          else (False, p)
   in case T.breakOn "." inner of
        (clazzTxt, dotNameTxt)
          | T.null dotNameTxt -> Left $ InvalidComplexTypeNoPeriod p
          | otherwise -> do
              clazz <- mkComplexClass clazzTxt
              let nameTxt = T.tail dotNameTxt
              name <- mkName nameTxt
              pure $ ComplexType isArray clazz name

mkComplexClass :: Text -> Either Error ComplexClass
mkComplexClass txt =
  case txt of
    "typedef" -> Right CTypedef
    "enum" -> Right CEnum
    "bitflag" -> Right CBitFlag
    "struct" -> Right CStruct
    "function_type" -> Right CFunction
    "object" -> Right CObject
    _ -> Left (InvalidComplexClass txt)

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
  showb (InvalidComplexClass input) =
    "Invalid ComplexType; class not recognised: '" <> showb input <> "'"
  showb (InvalidComplexTypeNoPeriod input) =
    "Invalid ComplexType; no period found: '" <> showb input <> "'"

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
    do
      checkNoExtraFields "Constant" ["name", "value", "doc"] o
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
  parseJSON = withObject "NamedEnumEntry" $ \o -> do
    checkNoExtraFields "NamedEnumEntry" ["name", "doc"] o
    EnumVariant
      <$> o .: "name"
      <*> o .: "doc"

instance ToJSON EnumEntry where
  toJSON NullEnumEntry = Null
  toJSON (EnumEntry v) = toJSON v

instance FromJSON EnumEntry where
  parseJSON Null = pure NullEnumEntry
  parseJSON (Object o) = EnumEntry <$> parseJSON (Object o)
  parseJSON _ = failText "EnumEntry must be a string or object"

instance ToJSON Enum where
  toJSON (Enum n d e) =
    object
      [ "name" .= n,
        "doc" .= d,
        "entries" .= e
      ]

instance FromJSON Enum where
  parseJSON = withObject "Enum" $ \o -> do
    checkNoExtraFields "Enum" ["name", "doc", "entries"] o
    Enum
      <$> o .: "name"
      <*> o .: "doc"
      <*> o .: "entries"

instance ToJSON BitFlagEntry where
  toJSON (BitFlagEntry n d v vc) =
    object $
      [ "name" .= n,
        "doc" .= d
      ]
        ++ maybe [] (\val -> ["value" .= val]) v
        ++ (["value_combination" .= vc | not (V.null vc)])

instance FromJSON BitFlagEntry where
  parseJSON = withObject "BitFlagEntry" $ \o -> do
    checkNoExtraFields "BitFlagEntry" ["name", "doc", "value", "value_combination"] o
    BitFlagEntry
      <$> o .: "name"
      <*> o .: "doc"
      <*> o .:? "value"
      <*> o .:? "value_combination" .!= V.empty

instance ToJSON BitFlag where
  toJSON (BitFlag n d e) =
    object
      [ "name" .= n,
        "doc" .= d,
        "entries" .= e
      ]

instance FromJSON BitFlag where
  parseJSON = withObject "BitFlag" $ \o -> do
    checkNoExtraFields "BitFlag" ["name", "doc", "entries"] o
    BitFlag
      <$> o .: "name"
      <*> o .: "doc"
      <*> o .: "entries"

instance ToJSON BaseType where
  toJSON b = String $ case b of
    Tbool -> "bool"
    Tstring Nothing -> "string"
    Tstring (Just StringNullable) -> "nullable_string"
    Tstring (Just StringWithDefaultEmpty) -> "string_with_default_empty"
    Tstring (Just OutString) -> "out_string"
    Tuint16 -> "uint16"
    Tuint32 -> "uint32"
    Tuint64 -> "uint64"
    Tusize -> "usize"
    Tint16 -> "int16"
    Tint32 -> "int32"
    Tfloat32 Nothing -> "float32"
    Tfloat32 (Just F32Nullable) -> "nullable_float32"
    Tfloat64 Nothing -> "float64"
    Tfloat64 (Just F64Supertype) -> "float64_supertype"

instance FromJSON BaseType where
  parseJSON (String s) = case s of
    "bool" -> pure Tbool
    "string" -> pure $ Tstring Nothing
    "nullable_string" -> pure $ Tstring (Just StringNullable)
    "string_with_default_empty" -> pure $ Tstring (Just StringWithDefaultEmpty)
    "out_string" -> pure $ Tstring (Just OutString)
    "uint16" -> pure Tuint16
    "uint32" -> pure Tuint32
    "uint64" -> pure Tuint64
    "usize" -> pure Tusize
    "int16" -> pure Tint16
    "int32" -> pure Tint32
    "float32" -> pure $ Tfloat32 Nothing
    "nullable_float32" -> pure $ Tfloat32 (Just F32Nullable)
    "float64" -> pure $ Tfloat64 Nothing
    "float64_supertype" -> pure $ Tfloat64 (Just F64Supertype)
    _ -> failText $ "Unknown PrimitiveType: " <> s
  parseJSON _ = failText "PrimitiveType must be a JSON string"

instance ToJSON ArrayType where
  toJSON (ArrayType b) =
    case toJSON b of
      String s -> String $ "array<" <> s <> ">"
      _ -> error "Base type does not serialize as a JSON String; cannot be used in an array."

instance FromJSON ArrayType where
  parseJSON (String s) =
    let p = T.strip s
     in case p of
          _
            | T.isPrefixOf "array<" p && T.isSuffixOf ">" p ->
                let b = T.init $ T.drop 6 p
                 in ArrayType <$> parseJSON (String b)
          _ -> failText "Array types must be of the form: array<...>"
  parseJSON _ = failText "ArrayType must be a JSON string"

instance ToJSON PrimitiveType where
  toJSON PTVoid = String "c_void"
  toJSON (PTBase b) = toJSON b
  toJSON (PTArray a) = toJSON a

instance FromJSON PrimitiveType where
  parseJSON (String s) = case T.strip s of
    "c_void" -> pure PTVoid
    p
      | T.isPrefixOf "array<" p && T.isSuffixOf ">" p -> PTArray <$> parseJSON (String p)
      | otherwise -> PTBase <$> parseJSON (String p)
  parseJSON _ = failText "PrimitiveType must be a JSON string"

instance ToJSON Pointer where
  toJSON PtrImmutable = String "immutable"
  toJSON PtrMutable = String "mutable"

instance FromJSON Pointer where
  parseJSON (String s) = case T.strip s of
    "immutable" -> pure PtrImmutable
    "mutable" -> pure PtrMutable
    p -> failText $ "Unknown pointer value: " <> p
  parseJSON _ = failText "Pointer must be a JSON string"

instance ToJSON ComplexType where
  toJSON (ComplexType a c n) =
    let (prefix, suffix) = if a then ("array<", ">") else (T.empty, T.empty)
        clazz = case c of
          CTypedef -> "typedef"
          CEnum -> "enum"
          CBitFlag -> "bitflag"
          CStruct -> "struct"
          CFunction -> "function_type"
          CObject -> "object"
        name = unName n
     in String $ prefix <> clazz <> "." <> name <> suffix

instance FromJSON ComplexType where
  parseJSON = parseWith mkComplexType

---- HELPER FUNCTIONS -----------------------------------------------------------------------------

-- | Checks that a YAML object has no unexpected fields.
checkNoExtraFields :: Text -> [Text] -> Object -> Parser ()
checkNoExtraFields objName expectedKeys o = do
  let isUnexpectedKey k = not $ Set.member k (Set.fromList expectedKeys)
  let unexpectedKeys = sort $ filter isUnexpectedKey (Key.toText <$> keys o)
  unless (null unexpectedKeys) $ do
    failText $ "Unexpected fields in " <> objName <> ": " <> showt unexpectedKeys

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
