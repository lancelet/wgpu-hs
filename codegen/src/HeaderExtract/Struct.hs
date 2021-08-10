{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

-- |
module HeaderExtract.Struct
  ( -- * Types
    Struct (Struct),
    Member (Member),
    TypeName (TypeName),

    -- * Functions
    collect,
  )
where

import Data.Maybe (mapMaybe)
import Data.String (IsString (fromString))
import Data.Text (Text)
import qualified Language.C as C
import qualified Language.C.Data.Ident as C

-------------------------------------------------------------------------------
-- Types

data Struct = Struct !Text [Member] deriving (Eq, Show)

data Member = Member !TypeName !Text deriving (Eq, Show)

newtype TypeName = TypeName Text deriving (Eq, Show, IsString)

-------------------------------------------------------------------------------
-- Functions

collect :: [C.CExternalDeclaration a] -> [Struct]
collect = mapMaybe matchStruct

matchStruct :: C.CExternalDeclaration a -> Maybe Struct
matchStruct =
  \case
    ( C.CDeclExt
        ( C.CDecl
            [ C.CStorageSpec (C.CTypedef _),
              C.CTypeSpec
                ( C.CSUType
                    ( C.CStruct
                        C.CStructTag
                        (Just (C.Ident name _ _))
                        (Just members)
                        _
                        _
                      )
                    _
                  )
              ]
            _
            _
          )
      ) -> Just (Struct (fromString name) (mapMaybe matchMember members))
    _ -> Nothing

matchMember :: C.CDeclaration a -> Maybe Member
matchMember =
  \case
    C.CDecl
      (C.CTypeSpec (C.CTypeDef (C.Ident typeName _ _) _) : _)
      [(Just (C.CDeclr (Just (C.Ident name _ _)) _ _ _ _), _, _)]
      _ -> Just (Member (fromString typeName) (fromString name))
    _ -> Nothing
