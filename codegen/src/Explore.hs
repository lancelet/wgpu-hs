module Explore where

import Control.Monad (void)
import qualified HeaderExtract.EnumW32 as EnumW32
import qualified HeaderExtract.Struct as Struct
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

explore :: IO ()
explore = do
  result <- parseCFile (newGCC "gcc") Nothing [] "webgpu-headers/webgpu.h"
  let Right rtu = result
  let CTranslUnit extdecls _ = rtu
  let eds = fmap void extdecls
  let es = EnumW32.collect eds
  let ss = Struct.collect eds
  pPrint es
  pPrint ss
