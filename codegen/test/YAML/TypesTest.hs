{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module YAML.TypesTest (tests) where

import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Vector qualified as V
import Data.Yaml (FromJSON, Object, ToJSON (toJSON), Value (Object, String), decodeEither', encode)
import Hedgehog (Gen, Property, PropertyT, failure, footnote, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)
import YAML.Types
  ( ArrayType (..),
    BaseType (..),
    BitFlag (..),
    BitFlagEntry (..),
    Constant (..),
    Enum (..),
    EnumEntry (EnumEntry, NullEnumEntry),
    EnumVariant (..),
    F32Nullable (..),
    F64Supertype (..),
    Name,
    StringHint (..),
    Value64 (..),
    mkName,
    nameQQ,
  )
import Prelude hiding (Enum)

tests :: TestTree
tests =
  testGroup
    "YAML.Types"
    [ testProperty "Value64 roundtrip" prop_value64_roundtrip,
      testProperty "Constant roundtrip" prop_constant_roundtrip,
      testProperty "EnumVariant roundtrip" prop_enum_variant_roundtrip,
      testProperty "EnumEntry roundtrip" prop_enum_entry_roundtrip,
      testProperty "Enum roundtrip" prop_enum_roundtrip,
      testProperty "BitFlagEntry roundtrip" prop_bit_flag_entry_roundtrip,
      testProperty "BitFlag roundtrip" prop_bit_flag_roundtrip,
      testProperty "BaseType roundtrip" prop_base_type_roundtrip,
      testProperty "ArrayType roundtrip" prop_array_type_roundtrip,
      testCase "Example: Constant parse" test_constant_parse,
      testCase "Example: Constant extra fields are rejected" test_constant_extra_fields_rejected,
      testCase "Example: Enum variant parse" test_enum_variant_parse,
      testCase "Example: Enum parse" test_enum_parse,
      testCase "Example: BitFlag parse" test_bitflag_parse
    ]

---- GENERATORS -----------------------------------------------------------------------------------

genValue64 :: Gen Value64
genValue64 =
  Gen.choice
    [ V64Number <$> Gen.word64 Range.linearBounded,
      pure V64UsizeMax,
      pure V64Uint32Max,
      pure V64Uint64Max,
      pure V64Nan
    ]

genName :: Gen Name
genName = fromRight . mkName <$> Gen.text (Range.linear 1 50) Gen.alphaNum

genDoc :: Gen Text
genDoc = Gen.text (Range.linear 1 500) Gen.alphaNum

genConstant :: Gen Constant
genConstant = Constant <$> genName <*> genValue64 <*> genDoc

genEnumVariant :: Gen EnumVariant
genEnumVariant = EnumVariant <$> genName <*> genDoc

genEnumEntry :: Gen EnumEntry
genEnumEntry =
  Gen.frequency
    [ (1, pure NullEnumEntry),
      (10, EnumEntry <$> genEnumVariant)
    ]

genEnum :: Gen Enum
genEnum = Enum <$> genName <*> genDoc <*> genEntries
  where
    genEntries = V.fromList <$> Gen.list (Range.linear 1 10) genEnumEntry

genBitFlagEntry :: Gen BitFlagEntry
genBitFlagEntry = BitFlagEntry <$> genName <*> genDoc <*> Gen.maybe genValue64 <*> genValComb
  where
    genValComb = V.fromList <$> Gen.list (Range.linear 1 10) genName

genBitFlag :: Gen BitFlag
genBitFlag = BitFlag <$> genName <*> genDoc <*> genEntries
  where
    genEntries = V.fromList <$> Gen.list (Range.linear 1 32) genBitFlagEntry

genBaseType :: Gen BaseType
genBaseType =
  Gen.choice
    [ pure Tbool,
      genString,
      pure Tuint16,
      pure Tuint32,
      pure Tuint64,
      pure Tusize,
      pure Tint16,
      pure Tint32,
      genFloat32,
      genFloat64
    ]
  where
    genStringHint =
      Gen.choice
        [ pure StringNullable,
          pure StringWithDefaultEmpty,
          pure OutString
        ]
    genString = Tstring <$> Gen.maybe genStringHint
    genFloat32 = Tfloat32 <$> Gen.maybe (pure F32Nullable)
    genFloat64 = Tfloat64 <$> Gen.maybe (pure F64Supertype)

genArrayType :: Gen ArrayType
genArrayType = ArrayType <$> genBaseType

---- PROPERTIES -----------------------------------------------------------------------------------

prop_value64_roundtrip :: Property
prop_value64_roundtrip = mkPropRoundtrip genValue64

prop_constant_roundtrip :: Property
prop_constant_roundtrip = mkPropRoundtrip genConstant

prop_enum_variant_roundtrip :: Property
prop_enum_variant_roundtrip = mkPropRoundtrip genEnumVariant

prop_enum_entry_roundtrip :: Property
prop_enum_entry_roundtrip = mkPropRoundtrip genEnumEntry

prop_enum_roundtrip :: Property
prop_enum_roundtrip = mkPropRoundtrip genEnum

prop_bit_flag_entry_roundtrip :: Property
prop_bit_flag_entry_roundtrip = mkPropRoundtrip genBitFlagEntry

prop_bit_flag_roundtrip :: Property
prop_bit_flag_roundtrip = mkPropRoundtrip genBitFlag

prop_base_type_roundtrip :: Property
prop_base_type_roundtrip = mkPropRoundtrip genBaseType

prop_array_type_roundtrip :: Property
prop_array_type_roundtrip = mkPropRoundtrip genArrayType

---- UNIT TESTS -----------------------------------------------------------------------------------

test_constant_parse :: IO ()
test_constant_parse = do
  let yamlInput =
        """
        name: copy_stride_undefined
        value: uint32_max
        doc: |
          Indicates no copy stride is specified. For more info,
          see @ref SentinelValues and the places that use this sentinel value.
        """
  let expected =
        Constant
          { name = [nameQQ|copy_stride_undefined|],
            value = V64Uint32Max,
            doc =
              T.intercalate
                "\n"
                [ "Indicates no copy stride is specified. For more info,",
                  "see @ref SentinelValues and the places that use this sentinel value."
                ]
          }
  shouldParseAs expected yamlInput

test_constant_extra_fields_rejected :: IO ()
test_constant_extra_fields_rejected = do
  let yamlInput =
        """
        name: copy_stride_undefined
        value: uint32_max
        foo: bar
        bar: baz
        doc: |
          Indicates no copy stride is specified. For more info,
          see @ref SentinelValues and the places that use this sentinel value.
        """
  case decodeEither' yamlInput of
    Left err -> do
      let e =
            "AesonException \"Error in $: Unexpected fields in Constant: "
              ++ "[\\\"bar\\\",\\\"foo\\\"]\""
      show err @?= e
    Right (_ :: Constant) -> error "Expected a parse failure due to extra fields."

test_enum_variant_parse :: IO ()
test_enum_variant_parse = do
  let yamlInput =
        """
        name: discrete_GPU
        doc: Indicates a discrete GPU.
        """
  let expected = EnumVariant [nameQQ|discrete_GPU|] "Indicates a discrete GPU."
  shouldParseAs expected yamlInput

test_enum_parse :: IO ()
test_enum_parse = do
  let yamlInput =
        """
        name: texture_view_dimension
        doc: Texture View Dimension
        entries:
          - null
          - name: undefined
            doc: Indicates no value
          - name: 1D
            doc: 1D Texture Dimension
          - name: 2D
            doc: 2D Texture Dimension
        """
  let expected =
        Enum
          { name = [nameQQ|texture_view_dimension|],
            doc = "Texture View Dimension",
            entries =
              V.fromList
                [ NullEnumEntry,
                  EnumEntry (EnumVariant [nameQQ|undefined|] "Indicates no value"),
                  EnumEntry (EnumVariant [nameQQ|1D|] "1D Texture Dimension"),
                  EnumEntry (EnumVariant [nameQQ|2D|] "2D Texture Dimension")
                ]
          }
  shouldParseAs expected yamlInput

test_bitflag_parse :: IO ()
test_bitflag_parse = do
  let yamlInput =
        """
        name: buffer_usage
        doc: Buffer usage
        entries:
          - name: map_read
            value: 42
            doc: Buffer mapped on CPU side for reads.
          - name: map_write
            doc: Buffer mapped on CPU side for writes.
          - name: map_rw
            doc: Read or write.
            value_combination:
              - map_read
              - map_write
        """
  let expected =
        BitFlag
          { name = [nameQQ|buffer_usage|],
            doc = "Buffer usage",
            entries =
              V.fromList
                [ BitFlagEntry
                    { name = [nameQQ|map_read|],
                      doc = "Buffer mapped on CPU side for reads.",
                      value = Just (V64Number 42),
                      valueCombination = V.empty
                    },
                  BitFlagEntry
                    { name = [nameQQ|map_write|],
                      doc = "Buffer mapped on CPU side for writes.",
                      value = Nothing,
                      valueCombination = V.empty
                    },
                  BitFlagEntry
                    { name = [nameQQ|map_rw|],
                      doc = "Read or write.",
                      value = Nothing,
                      valueCombination =
                        V.fromList
                          [ [nameQQ|map_read|],
                            [nameQQ|map_write|]
                          ]
                    }
                ]
          }
  shouldParseAs expected yamlInput

---- HELPER FUNCTIONS -----------------------------------------------------------------------------

-- | Create a comprehensive Hedgehog property test for YAML serialization.
--
-- This function generates a property test that verifies both correctness and
-- strictness of YAML serialization for a given type. It combines two critical
-- tests into a single property:
--
-- 1. **Roundtrip Test**: Verifies that values can be encoded to YAML and
--    decoded back to exactly the original value, ensuring no data is lost
--    or corrupted in the serialization process.
--
-- 2. **Strict Parsing Test**: For object types, verifies that the parser
--    rejects JSON/YAML with unexpected fields, ensuring schema compliance
--    and helping catch field name typos early.
--
-- The test uses the custom 'trippingYAML' function for better error diagnostics
-- than the standard Hedgehog 'tripping' function.
mkPropRoundtrip :: (Show a, Eq a, ToJSON a, FromJSON a) => Gen a -> Property
mkPropRoundtrip gen = property $ do
  -- Check regular round-trip parsing.
  forAll gen >>= trippingYAML
  -- Check that any extra fields on a serialized object reject it.
  propRejectsExtraFields gen

-- | Verify that FromJSON instances reject objects with unexpected fields.
--
-- This property test ensures our parsers implement strict field checking,
-- failing when they encounter fields not explicitly handled by the parser.
-- This helps catch typos in field names and ensures schema compliance.
--
-- The test works by:
-- 1. Generating a value and converting it to JSON
-- 2. If it's an object, injecting fake fields with obvious names
-- 3. Attempting to parse the modified JSON back to the original type
-- 4. Asserting that parsing fails due to the unexpected fields
--
-- Non-object JSON values (arrays, primitives) are skipped since they
-- don't have fields to validate.
propRejectsExtraFields ::
  forall a m.
  (Show a, ToJSON a, FromJSON a, Monad m) =>
  Gen a -> PropertyT m ()
propRejectsExtraFields gen = do
  -- Convert the value fo JSON
  value <- forAll gen
  let json = toJSON value
  case json of
    Object obj' -> do
      -- Generate additional key-value pairs.
      let genKV = do
            k' <- Gen.text (Range.linear 1 10) Gen.alphaNum
            let k = k' <> "__FAKE_FIELD__"
            v <- String <$> Gen.text (Range.linear 1 10) Gen.alphaNum
            pure (k, v)
      let genKVList = Gen.list (Range.linear 1 5) genKV
      kvs <- forAll genKVList

      -- Add the key-value pairs to the object.
      let obj = addPairs kvs obj'

      -- Encode the object with extra fake fields.
      let encoded = encode obj

      -- Ensure that decoding the object with fake fields fails.
      case decodeEither' encoded of
        Right (_ :: a) -> do
          footnote $ "Expected decode to fail. Object: " <> T.unpack (decodeUtf8 encoded)
          failure
        Left _ -> pure ()

    -- Skip the test if we didn't get a JSON object.
    _ -> pure ()

-- | Add key-value pairs to an object.
addPairs :: [(Text, Value)] -> Object -> Object
addPairs kvs o = foldr (\(k, v) acc -> KM.insert (Key.fromText k) v acc) o kvs

-- | Assert that a YAML ByteString parses to the expected value.
--
-- This function attempts to parse the given YAML input and compares it against
-- the expected value using HUnit's '@?=' assertion. If parsing fails, it throws
-- an error with the parse error details.
shouldParseAs ::
  forall a.
  (FromJSON a, Eq a, Show a) =>
  -- | Expected value.
  a ->
  -- | Input YAML.
  ByteString ->
  -- | IO test action.
  IO ()
shouldParseAs expected yamlInput =
  case decodeEither' yamlInput of
    Left err -> error $ "Failed to parse YAML: " <> show err
    Right (actual :: a) -> actual @?= expected

-- | Test that a value successfully roundtrips through YAML encoding and decoding.
--
-- This is a specialized version of Hedgehog's 'tripping' function designed for YAML/JSON
-- serialization testing. It encodes a value to YAML and then decodes it back, verifying
-- that the original value is recovered.
--
-- Unlike the standard 'tripping' which uses 'Maybe' for the decoder, this function works
-- with 'Data.Yaml.decodeEither'' which returns @'Either' 'ParseException' a@, providing
-- better error diagnostics.
trippingYAML :: (Show a, Eq a, ToJSON a, FromJSON a, Monad m) => a -> PropertyT m ()
trippingYAML value = do
  let encoded = encode value
  footnote $ "Encoded: " <> show encoded
  case decodeEither' encoded of
    Left err -> do
      footnote $ "Decode error: " <> show err
      failure
    Right decoded -> value === decoded

-- | Unwraps a `Right` value, producing an error on `Left`.
fromRight :: (Show a) => Either a b -> b
fromRight (Right x) = x
fromRight (Left x) = error $ "Expected Right, but got (Left " <> show x <> ")"