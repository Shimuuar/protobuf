-- | Haskell code generator
module Data.Protobuf.CodeGen where

import Data.List
import Data.Protobuf.AST
import Data.Protobuf.DataTree

import Language.Haskell.Exts.Syntax


-- | Convert module into the haskell code to be dumped
convert :: ([Identifier],HsModule) -> Module
convert (qs, msg) = 
  Module s (modName qs) 
   [ LanguagePragma s [ Ident "DeriveDataTypeable"
                      , Ident "NoImplicitPrelude"
                      ] ]
   Nothing Nothing 
   -- Imports
   [ ImportDecl { importLoc       = s 
                , importModule    = ModuleName "Data.Protobuf.Imports"
                , importQualified = True
                , importSrc       = False
                , importPkg       = Nothing
                , importAs        = Just $ ModuleName "P'"
                , importSpecs     = Nothing
                }
   ] 
   -- Code
   (convertDecl msg)


-- | Convert declaration
convertDecl :: HsModule -> [Decl]
convertDecl (HsMessage (TyName name) fields) = 
  -- Data declaration
  [ DataDecl s DataType [] (Ident name) []
      [ QualConDecl s [] [] $ RecDecl (Ident name) (map recordField fields)
      ]
      derives
  -- ,
  ]
convertDecl (HsEnum    (TyName name) fields) = 
  -- Data declaration
  [ DataDecl s DataType [] (Ident name) [] 
      -- Constructors
      [ QualConDecl s [] [] (ConDecl (Ident n) []) | (TyName n, _) <- fields ]
      -- Deriving clause
      ( (qname "Eq", []) : derives )
  -- PbEnum instance
  , declInstance  "PbEnum" name
      -- fromPbEnum
      [ instFun "fromPbEnum"
        [ ( [PVar $ Ident n]
          , Lit $ Int i)
        | (TyName n, i) <- fields ]
      -- toPbEnum
      , instFun "toPbEnum" 
        [ ( [PLit $ Int i]
          , Con $ UnQual $ Ident n)
        | (TyName n, i) <- fields ]
      ]
  -- Ord instance
  , declInstance "Ord" name
    [ instFun "compare" 
        [ ( []
          , (Var $ qname "comparing") `App` (Var $ qname "fromPbEnum"))
        ]
    ]
  ]

derives = 
  [ (qname "Show",     [])
  , (qname "Typeable", [])
  , (qname "Data",     [])
  ]

-- | Single fields of record
recordField :: HsField -> ([Name], BangType)
recordField (HsField tp name _) = 
  ([Ident name], outerType tp)
  where
    outerType (HsSimple t) = BangedTy (innerType t)
    outerType (HsMaybe  t) = BangedTy (innerType t) -- !
    outerType (HsSeq    t) = BangedTy (innerType t) -- !
    
    innerType (HsBuiltin  t) = primType t
    innerType (HsUserType (Qualified qs n)) =
      TyCon $ Qual (modName (qs++[n])) (Ident $ identifier n)
    
    primType PbDouble   = TyCon $ qname "Double"
    primType PbFloat    = TyCon $ qname "Float"
    primType PbInt32    = sint32
    primType PbInt64    = sint64
    primType PbUInt32   = uint32
    primType PbUInt64   = uint64
    primType PbSInt32   = sint32
    primType PbSInt64   = sint64
    primType PbFixed32  = uint32
    primType PbFixed64  = uint64
    primType PbSFixed32 = sint32
    primType PbSFixed64 = sint64
    primType PbBool     = TyCon $ qname "Bool"
    primType PbString   = TyCon $ qname "String"
    primType PbBytes    = TyCon $ qname "Bytestring"

    sint32 = TyCon $ qname "Int32"
    sint64 = TyCon $ qname "Int64"
    uint32 = TyCon $ qname "Word32"
    uint64 = TyCon $ qname "Word64"



----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

modName :: [Identifier] -> ModuleName
modName = ModuleName . intercalate "." . map identifier

-- Function declaration in the instance
instFun name decls =
  InsDecl $ FunBind 
    [ Match s (Ident name)
      pat Nothing (UnGuardedRhs rhs) (BDecls [])
    | (pat,rhs) <- decls
    ]

s :: SrcLoc
s =  SrcLoc "" 0 0

qname = Qual (ModuleName "P'") . Ident

declInstance cl name =
  InstDecl s [] (qname cl) [TyCon $ UnQual $ Ident name]
