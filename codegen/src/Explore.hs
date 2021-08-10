module Explore where

import Control.Monad (void)
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
import Language.C.System.GCC (newGCC)
import Text.Pretty.Simple (pPrint)

import qualified HeaderExtract.EnumW32 as EnumW32

explore :: IO ()
explore = do
  result <- parseCFile (newGCC "gcc") Nothing [] "webgpu-headers/webgpu.h"
  let Right rtu = result
  let CTranslUnit extdecls _ = rtu
  let eds = fmap void extdecls
  -- pPrint eds
  let es = EnumW32.collect eds
  pPrint es

