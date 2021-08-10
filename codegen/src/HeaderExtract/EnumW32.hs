{-# LANGUAGE LambdaCase #-}

-- | Extract 32-bit integer enums from the webgpu header file.
--
-- These typically look like this:
--
-- @
-- typedef enum Name {
--   Entry1 = 0x00000000,
--   Entry2 = 0x00000001
--   /* ... */
-- }
-- @
module HeaderExtract.EnumW32
  ( -- * Types
    EnumW32 (EnumW32),
    Entry (Entry),

    -- * Functions
    collect,
  )
where

import Data.Maybe (mapMaybe)
import Data.Text as Text
import Data.Word (Word32)
import qualified Language.C as C
import qualified Language.C.Data.Ident as C

-------------------------------------------------------------------------------
-- Types

-- | Enumeration containing Word32 entries.
data EnumW32 = EnumW32 !Text [Entry] deriving (Eq, Show)

-- | Entry in an enumeration.
data Entry = Entry !Text !Word32 deriving (Eq, Show)

-------------------------------------------------------------------------------
-- Functions

collect :: [C.CExternalDeclaration a] -> [EnumW32]
collect = mapMaybe matchEnumW32

matchEnumW32 :: C.CExternalDeclaration a -> Maybe EnumW32
matchEnumW32 =
  \case
    C.CDeclExt
      ( C.CDecl
          [ C.CStorageSpec (C.CTypedef _),
            C.CTypeSpec
              ( C.CEnumType
                  ( C.CEnum
                      (Just (C.Ident name _ _))
                      (Just entryList)
                      _
                      _
                    )
                  _
                )
            ]
          _
          _
        ) ->
        Just
          ( EnumW32
              (Text.pack name)
              (mapMaybe matchEntry entryList)
          )
    _ -> Nothing

matchEntry :: (C.Ident, Maybe (C.CExpression a)) -> Maybe Entry
matchEntry =
  \case
    ( C.Ident key _ _,
      Just (C.CConst (C.CIntConst (C.CInteger value _ _) _))
      ) -> Just (Entry (Text.pack key) (fromInteger value))
    _ -> Nothing
