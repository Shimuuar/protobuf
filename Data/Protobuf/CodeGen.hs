-- | Haskell code generator
module Data.Protobuf.CodeGen where

import Data.List
import Data.Protobuf.AST
import Data.Protobuf.DataTree

import Language.Haskell.Exts.Syntax



convert :: ([Identifier],HsModule) -> Module
convert (qs, msg) = 
-- convert (qs, HsMessage nm fields) = 
  Module s (modName qs) [] Nothing Nothing 
  [ ] 
  (convertDecl msg)
-- convert (qs, HsEnum nm fields) =
--   Module s (modName qs) [] Nothing Nothing 
--   [] []

convertDecl :: HsModule -> [Decl]
convertDecl (HsMessage (TyName name) fields) = 
  -- Data declaration
  [ DataDecl s DataType [] (Ident name) []
      [ QualConDecl s [] [] $ RecDecl (Ident name) (map recordField fields)
      ]
      []
  ]
convertDecl (HsEnum    (TyName name) fields) = 
  -- Data declaration
  [ DataDecl s DataType [] (Ident name) [] 
      -- Constructors
      [ QualConDecl s [] [] (ConDecl (Ident n) []) | (TyName n, _) <- fields ]
      -- Deriving clause
      [ ]
  -- Ord instance
  , InstDecl s [] (UnQual (Ident "PbEnum")) [TyCon $ UnQual $ Ident name]
      -- fromPbEnum
      [ InsDecl $ FunBind 
        [ Match s (Ident "fromPbEnum") 
          [PVar $ Ident n] 
          Nothing (UnGuardedRhs $ Lit $ Int i) (BDecls [])
        | (TyName n, i) <- fields ]
      -- toPbEnum
      , InsDecl $ FunBind $
        [ Match s (Ident "fromPbEnum") 
          [PLit $ Int i]
          Nothing (UnGuardedRhs $ Con $ UnQual $ Ident n) (BDecls [])
        | (TyName n, i) <- fields ]
        -- FIXME: Produce error!
      ]
  ]

modName :: [Identifier] -> ModuleName
modName = ModuleName . intercalate "." . map identifier

modulePrefix :: Qualified Identifier -> ModuleName
modulePrefix (Qualified qs n) = modName $ qs ++ [n]

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
    
    primType PbDouble   = TyCon $ UnQual $ Ident "Double"
    primType PbFloat    = TyCon $ UnQual $ Ident "Float"
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
    primType PbBool     = TyCon $ UnQual $ Ident "Bool"
    primType PbString   = TyCon $ UnQual $ Ident "String"
    primType PbBytes    = TyCon $ UnQual $ Ident "Bytestring"

    sint32 = TyCon $ UnQual $ Ident "Int32"
    sint64 = TyCon $ UnQual $ Ident "Int64"
    uint32 = TyCon $ UnQual $ Ident "Word32"
    uint64 = TyCon $ UnQual $ Ident "Word64"
  
s :: SrcLoc
s =  SrcLoc "" 0 0