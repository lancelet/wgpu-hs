{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module YAML.TypesTest (tests) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Yaml (FromJSON, ToJSON, decodeEither', encode)
import Hedgehog (Gen, Property, PropertyT, failure, footnote, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)
import YAML.Types
  ( Constant (..),
    Enum (..),
    EnumEntry (EnumEntry, NullEnumEntry),
    EnumVariant (..),
    Name,
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
      testCase "Example: Constant parse" test_constant_parse,
      testCase "Example: Enum variant parse" test_enum_variant_parse,
      testCase "Example: Enum parse" test_enum_parse
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

---- HELPER FUNCTIONS -----------------------------------------------------------------------------

-- | Create a Hedgehog property test for YAML serialization roundtrips.
--
-- This function takes a Hedgehog generator and creates a property test that verifies
-- values can be successfully encoded to YAML and then decoded back to the original
-- value. It uses the custom 'trippingYAML' function which provides better error
-- diagnostics than the standard Hedgehog 'tripping' function.
--
-- The generated property will:
--
-- 1. Generate random values using the provided generator
-- 2. Serialize each value to YAML using 'toJSON' and 'encode'
-- 3. Deserialize back using 'decodeEither''
-- 4. Assert that the original and deserialized values are equal
mkPropRoundtrip :: (Show a, Eq a, ToJSON a, FromJSON a) => Gen a -> Property
mkPropRoundtrip gen = property $ forAll gen >>= trippingYAML

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