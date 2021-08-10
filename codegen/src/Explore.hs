module Explore where

import Control.Monad (void)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word32)
import Language.C
  ( CConstant (CIntConst),
    CDeclaration (CDecl),
    CDeclarationSpecifier (CStorageSpec, CTypeSpec),
    CEnumeration (CEnum),
    CExpression (CConst),
    CExternalDeclaration (CDeclExt),
    CInteger (CInteger),
    CStorageSpecifier (CTypedef),
    CTranslationUnit (CTranslUnit),
    CTypeSpecifier (CEnumType),
    parseCFile,
  )
import Language.C.Data.Ident (Ident (Ident))
import Language.C.System.GCC (newGCC)
import Text.Pretty.Simple (pPrint)

explore :: IO ()
explore = do
  result <- parseCFile (newGCC "gcc") Nothing [] "webgpu-headers/webgpu.h"
  let Right rtu = result
  let CTranslUnit extdecls _ = rtu
  let eds = fmap void extdecls
  pPrint eds
  let es = enums eds
  pPrint es

enums :: [CExternalDeclaration a] -> [EnumW32]
enums = mapMaybe matchEnumW32

data EnumW32 = EnumW32 !Text [(Text, Word32)] deriving (Show)

matchEnumW32 :: CExternalDeclaration a -> Maybe EnumW32
matchEnumW32 d =
  case d of
    CDeclExt
      ( CDecl
          [ CStorageSpec (CTypedef _),
            CTypeSpec
              ( CEnumType
                  ( CEnum
                      (Just (Ident name _ _))
                      (Just itemList)
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
              (mapMaybe matchEnumW32Member itemList)
          )
    _ -> Nothing

matchEnumW32Member :: (Ident, Maybe (CExpression a)) -> Maybe (Text, Word32)
matchEnumW32Member tpl =
  case tpl of
    ( Ident key _ _,
      Just (CConst (CIntConst (CInteger value _ _) _))
      ) ->
        Just (Text.pack key, fromInteger value)
    _ -> Nothing
