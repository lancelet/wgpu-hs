{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tools for parsing @wgpu.h@.
--
-- This module uses <https://hackage.haskell.org/package/language-c language-c>
-- to parse @wgpu.h@. It consists of distilling the API declared in the C header
-- file(s) for WGPU into a simpler representation than language-c can provide.
-- The simpler representation only contains a representation of the C components
-- that is sophisticated enough to represent the requirements of WGPU.
--
-- The returned API and its elements are 'Hashable', so that changes are easier
-- to detect.
module WGPU.CodeGen.Parse
  ( -- * Types

    -- ** API
    CApi (CApi, cApiEnums, cApiStructs, cApiFuns),

    -- ** Word32 enums
    CEnumW32 (CEnumW32),
    CEnumW32Member (CEnumW32Member),

    -- ** C Types
    CType (CPtr, CVoid, CBool, CChar, CInt, CFloat, CDouble, CDefined),

    -- ** C Structs
    CStruct (CStruct),
    CStructMember (CStructMember),

    -- ** C Functions
    CFun (CFun),
    CFunParam (CFunParam),

    -- * Functions
    readApi,
  )
where

import Control.Monad (unless)
import Data.Hashable (Hashable (hashWithSalt))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word32)
import GHC.Generics (Generic)
import Language.C
  ( CConstant (CIntConst),
    CExpression (CConst),
    CInteger (CInteger),
    SUERef (AnonymousRef, NamedRef),
    parseCFile,
  )
import Language.C.Analysis
  ( CompTyKind (StructTag),
    CompType (CompType),
    CompTypeRef (CompTypeRef),
    Decl (Decl),
    EnumType (EnumType),
    Enumerator (Enumerator),
    FloatType (TyDouble, TyFloat),
    FunType (FunType, FunTypeIncomplete),
    GlobalDecls (gObjs),
    IdentDecl (Declaration),
    IntType (TyBool, TyChar, TyInt),
    MemberDecl (AnonBitField, MemberDecl),
    ParamDecl (AbstractParamDecl, ParamDecl),
    TagDef (CompDef, EnumDef),
    Type (DirectType, FunctionType, PtrType, TypeDefType),
    TypeDefRef (TypeDefRef),
    TypeName (TyComp, TyFloating, TyIntegral, TyVoid),
    VarDecl (VarDecl),
    VarName (NoName, VarName),
    gTags,
  )
import Language.C.Analysis.AstAnalysis (analyseAST)
import Language.C.Analysis.TravMonad (runTrav_)
import Language.C.Data.Ident (Ident (Ident))
import Language.C.System.GCC (newGCC)
import qualified System.Exit as System

-------------------------------------------------------------------------------

-- | Minimal representation of the C API.
data CApi = CApi
  { -- | Enumerations in the API. These are all 'Word32' values.
    cApiEnums :: Map Text CEnumW32,
    -- | Structures in the API.
    cApiStructs :: Map Text CStruct,
    -- | Declared functions in the API.
    cApiFuns :: Map Text CFun
  }
  deriving (Eq, Show, Generic)

instance Hashable CApi where
  hashWithSalt salt cApi =
    let enums :: [CEnumW32]
        enums = fmap snd . Map.toAscList . cApiEnums $ cApi

        structs :: [CStruct]
        structs = fmap snd . Map.toAscList . cApiStructs $ cApi

        fns :: [CFun]
        fns = fmap snd . Map.toAscList . cApiFuns $ cApi
     in salt
          `hashWithSalt` enums
          `hashWithSalt` structs
          `hashWithSalt` fns

-- | Read the API from wgpu.hs
readApi :: FilePath -> IO CApi
readApi filePath = do
  -- obtain the translation unit
  translUnit <- do
    r <- parseCFile (newGCC "gcc") Nothing [] filePath
    case r of
      Left err -> do
        putStrLn $ "Could not parse C file: " <> filePath
        print err
        System.exitFailure
      Right x -> pure x

  -- analyse the AST to extract declarations, etc.
  globalDecls <- do
    let r = runTrav_ $ analyseAST translUnit
    case r of
      Left errs -> do
        putStrLn $ "Terminal errors encountered in C file: " <> filePath
        print errs
        System.exitFailure
      Right (x, errs) -> do
        unless (null errs) $ do
          putStrLn $ "Non-terminal errors encountered in C file: " <> filePath
          print errs
        pure x

  -- fetch the AST
  let enums = getEnums globalDecls
      structs = getStructs globalDecls
      fns = getFunctions globalDecls

  -- produce maps
  let enumsMap = Map.fromList $ fmap (\e@(CEnumW32 k _) -> (k, e)) enums
      structsMap = Map.fromList $ fmap (\s@(CStruct k _) -> (k, s)) structs
      fnMap = Map.fromList $ fmap (\f@(CFun k _ _) -> (k, f)) fns

  pure $ CApi enumsMap structsMap fnMap

-------------------------------------------------------------------------------

getTags :: GlobalDecls -> [TagDef]
getTags gds = Map.elems $ gTags gds

getIdent :: Ident -> Text
getIdent (Ident str _ _) = Text.pack str

getName :: SUERef -> Text
getName sr =
  case sr of
    NamedRef ident -> getIdent ident
    AnonymousRef _ -> error "Unexpected anonymous SUERef"

-------------------------------------------------------------------------------

-- | A C enum containing only 'Word32' values.
data CEnumW32 = CEnumW32 !Text [CEnumW32Member]
  deriving (Eq, Show, Generic)

instance Hashable CEnumW32

-- | A member of a C 'Word32' enumeration.
data CEnumW32Member = CEnumW32Member !Text !Word32
  deriving (Eq, Show, Generic)

instance Hashable CEnumW32Member

getEnums :: GlobalDecls -> [CEnumW32]
getEnums gds =
  let mEnumType :: TagDef -> Maybe EnumType
      mEnumType tagDef =
        case tagDef of
          EnumDef x -> Just x
          CompDef _ -> Nothing

      enumTypes :: [EnumType]
      enumTypes = mapMaybe mEnumType (getTags gds)

      enumMember :: Enumerator -> CEnumW32Member
      enumMember enumerator =
        case enumerator of
          Enumerator
            (Ident str _ _)
            (CConst (CIntConst (CInteger x _ _) _))
            _
            _ -> CEnumW32Member (Text.pack str) (fromIntegral x)
          Enumerator {} -> error "Unexpected enum member pattern"

      convertEnum :: EnumType -> CEnumW32
      convertEnum (EnumType sueRef es _ _) =
        CEnumW32 (getName sueRef) (enumMember <$> es)
   in convertEnum <$> enumTypes

-------------------------------------------------------------------------------

-- | Type in the C API.
data CType
  = -- | A pointer to a type.
    CPtr !CType
  | -- | @void@.
    CVoid
  | -- | @bool@.
    CBool
  | -- | @char@.
    CChar
  | -- | @int@.
    CInt
  | -- | @float@.
    CFloat
  | -- | @double@.
    CDouble
  | -- | A type defined elsewhere that is being referenced by name.
    CDefined !Text
  deriving (Eq, Show, Generic)

instance Hashable CType

translateType :: Type -> CType
translateType t =
  case t of
    (TypeDefType (TypeDefRef (Ident str _ _) _ _) _ _) ->
      CDefined (Text.pack str)
    (PtrType tp _ _) ->
      CPtr (translateType tp)
    (DirectType TyVoid _ _) ->
      CVoid
    (DirectType (TyIntegral TyBool) _ _) ->
      CBool
    (DirectType (TyIntegral TyChar) _ _) ->
      CChar
    (DirectType (TyIntegral TyInt) _ _) ->
      CInt
    (DirectType (TyFloating TyFloat) _ _) ->
      CFloat
    (DirectType (TyFloating TyDouble) _ _) ->
      CDouble
    -- special case... handle WGPUChainedStruct.next
    ( DirectType
        (TyComp (CompTypeRef (NamedRef (Ident str _ _)) StructTag _))
        _
        _
      ) ->
        CDefined (Text.pack str)
    _ ->
      error $ "Unexpected type " <> show t

-------------------------------------------------------------------------------

-- | A C struct.
data CStruct = CStruct !Text [CStructMember]
  deriving (Eq, Show, Generic)

instance Hashable CStruct

-- | A member within a C struct.
data CStructMember = CStructMember !Text !CType
  deriving (Eq, Show, Generic)

instance Hashable CStructMember

getStructs :: GlobalDecls -> [CStruct]
getStructs gds =
  let mCompType :: TagDef -> Maybe CompType
      mCompType tagDef =
        case tagDef of
          CompDef x -> Just x
          EnumDef _ -> Nothing

      getSMName :: VarName -> Text
      getSMName vd =
        case vd of
          VarName (Ident str _ _) _ -> Text.pack str
          NoName -> error "Unexpected no-name struct member"

      structMember :: MemberDecl -> CStructMember
      structMember md =
        case md of
          AnonBitField {} -> error "Unexpected anonymous bit field"
          MemberDecl (VarDecl n _ t) _ _ ->
            CStructMember (getSMName n) (translateType t)

      mStruct :: CompType -> Maybe CompType
      mStruct ct =
        case ct of
          CompType _ StructTag _ _ _ -> Just ct
          _ -> Nothing

      -- hide structs that start with an underscore
      mVisible :: CompType -> Maybe CompType
      mVisible ct =
        case ct of
          CompType sueRef _ _ _ _ ->
            if "_" `Text.isPrefixOf` getName sueRef
              then Nothing
              else Just ct

      structs :: [CompType]
      structs =
        mapMaybe mVisible
          . mapMaybe mStruct
          . mapMaybe mCompType
          . getTags
          $ gds

      convertStruct :: CompType -> CStruct
      convertStruct ct =
        case ct of
          CompType sueRef StructTag ms _ _ ->
            CStruct (getName sueRef) (structMember <$> ms)
          _ -> error "Unexpected CompType format"
   in convertStruct <$> structs

-------------------------------------------------------------------------------

-- | A C function.
data CFun = CFun !Text !CType [CFunParam]
  deriving (Eq, Show, Generic)

instance Hashable CFun

-- | A parameter to a C function.
data CFunParam = CFunParam !Text !CType
  deriving (Eq, Show, Generic)

instance Hashable CFunParam

getVarName :: VarName -> Text
getVarName vn =
  case vn of
    VarName (Ident str _ _) _ -> Text.pack str
    NoName -> error "Unexpected VarName without a name"

getFunctions :: GlobalDecls -> [CFun]
getFunctions gds =
  let identDecls :: [IdentDecl]
      identDecls =
        Map.elems $
          Map.filterWithKey
            ( \(Ident str _ _) _ ->
                not ("_" `Text.isPrefixOf` (Text.pack str))
            )
            $ gObjs gds

      decls :: [Decl]
      decls = mapMaybe mDecl identDecls
        where
          mDecl :: IdentDecl -> Maybe Decl
          mDecl identDecl =
            case identDecl of
              Declaration decl -> Just decl
              _ -> Nothing

      getReturnType :: FunType -> CType
      getReturnType ft =
        case ft of
          FunType t _ _ -> translateType t
          FunTypeIncomplete _ -> error "Unexpected incomplete function type"

      getParams :: FunType -> [CFunParam]
      getParams ft =
        case ft of
          FunType _ ps _ -> translateParamDecl <$> ps
          FunTypeIncomplete _ -> error "Unexpected incomplete function type"

      translateParamDecl :: ParamDecl -> CFunParam
      translateParamDecl pd =
        case pd of
          AbstractParamDecl _ _ -> error "Unexpected abstract param decl"
          ParamDecl (VarDecl (VarName (Ident name _ _) _) _ t) _ ->
            CFunParam (Text.pack name) (translateType t)
          ParamDecl (VarDecl NoName _ _) _ ->
            error "Function parameter without a name"

      mFunDecl :: Decl -> Maybe CFun
      mFunDecl (Decl varDecl _) =
        case varDecl of
          VarDecl varName _ (FunctionType funType _) ->
            Just $
              CFun
                (getVarName varName)
                (getReturnType funType)
                (getParams funType)
          _ -> Nothing
   in mapMaybe mFunDecl decls
