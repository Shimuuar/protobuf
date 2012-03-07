{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Haskell code generator
module Data.Protobuf.CodeGen ( 
  convert
  ) where

-- import Control.Arrow
import Data.List
import Data.Protobuf.AST
import Data.Protobuf.DataTree

import Language.Haskell.Exts.Syntax
import Debug.Trace

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
  [ DataDecl s DataType [] (Ident name) [UnkindedVar $ Ident "r"]
      [ QualConDecl s [] [] $ RecDecl (Ident name) (map recordField fields)
      ]
      derives
  , instance_ "Default" (tycon name `TyApp` tycon "Required") 
      [ bind "def" =: foldl App (con name)
          [ case defV of 
              Just v  -> lit v 
              Nothing -> qvar "def" 
          | HsField _ _ _ defV <- fields ]
      ]
  , instance_ "Monoid" (tycon name `TyApp` tycon "Required")
      [ bind "mempty" =: qvar "def"
      , let (m1,m2) = unzip [ ( pvar $ "x" ++ show i
                              , pvar $ "y" ++ show i )
                            | (i,_) <- zip [0::Int ..] fields ]
        in fun "mappend" [ (PApp $ UnQual $ Ident name) m1
                         , (PApp $ UnQual $ Ident name) m2
                         ]
              =: foldl App (con name)
                      [ app [ qvar "mergeField"
                            , var $ "x" ++ show i
                            , var $ "y" ++ show i
                            ]
                      | (i, _) <- zip [0::Int ..] fields
                      ]
      ]
  , instance_ "Message" (tycon name)
      [ let q = ()
        in bind "getMessage" =:
             let_ [ bind "loop" =:
                    Lambda s [pvar "v"]
                    ( Do [ pvar "wt" <-- qvar "getWireTag"
                         , Qualifier $
                           Case (var "wt") $ 
                           concatMap caseField fields
                         ]
                    )
                  ]
               (var "loop" `App` qvar "mempty")
            -- (Var $ qname "mempty") 
      ]
  ]
convertDecl (HsEnum    (TyName name) fields) =
  -- Data declaration
  [ DataDecl s DataType [] (Ident name) []
      -- Constructors
      [ QualConDecl s [] [] (ConDecl (Ident n) []) | (TyName n, _) <- fields ]
      -- Deriving clause
      derives
  -- PbEnum instance
  , instance_ "PbEnum" (tycon name) $ 
      [ fun "fromPbEnum" [pvar n] =: lit i | (TyName n, i) <- fields ] ++
      [ fun "toPbEnum"   [plit i] =: con n | (TyName n, i) <- fields ]
  -- Ord instance
  , instance_ "Ord" (tycon name)
      [ bind "compare" =: app [ qvar "comparing"
                              , qvar "fromPbEnum" ]
      ]
  , instance_ "Default" (tycon name)
      [ bind "def" =: con (case head fields of { (TyName n,_) -> n })
      ]
  ]

derives = map (\n -> (qname n, []))
  [ "Show", "Eq", "Typeable", "Data" ]


-- | Single fields of record
recordField :: HsField -> ([Name], BangType)
recordField (HsField tp name _ _) =
  ([Ident name], outerType tp)
  where
    outerType (HsReq   t  ) = BangedTy (TyVar (Ident "r") `TyApp` innerType t)
    outerType (HsMaybe t  ) = BangedTy (innerType t) -- !
    outerType (HsSeq   t _) = BangedTy (innerType t) -- !

    innerType (HsBuiltin     t) = primType t
    innerType (HsUserMessage q) = userType q
    innerType (HsUserEnum    q) = userType q

    userType (Qualified qs n) = TyCon $ Qual (modName (qs++[n])) (Ident $ identifier n)

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


caseField (HsField ty name (FieldTag tag) _) =
  -- We have found tag
  [ Alt s (PApp (qname "WireTag") [plit tag, plit typeTag])
    (UnGuardedAlt $ 
     Do [ pvar "f" <-- getter
        , Qualifier $ qvar "undefined"
        ]
    )
    (BDecls [])
  -- Oops! wrong field type
  , Alt s (PApp (qname "WireTag") [plit tag, PWildCard])
    (UnGuardedAlt $ app [ qvar "fail"
                        , lit "Invalid tag!"
                        ]
    )
    (BDecls [])
  ]
  where
    -- Type tags
    typeTag = case ty of
      HsReq   t      -> innerTag t
      HsMaybe t      -> innerTag t
      HsSeq   t True -> lenDelim
      HsSeq   t _    -> innerTag t

    innerTag (HsUserMessage _) = lenDelim
    innerTag (HsUserEnum    _) = varint
    innerTag (HsBuiltin t)     = case t of
      PbDouble   -> fixed64
      PbFloat    -> fixed32
      PbInt32    -> varint
      PbInt64    -> varint
      PbUInt32   -> varint
      PbUInt64   -> varint
      PbSInt32   -> varint
      PbSInt64   -> varint
      PbFixed32  -> fixed32
      PbFixed64  -> fixed64
      PbSFixed32 -> fixed32
      PbSFixed64 -> fixed64
      PbBool     -> varint
      PbString   -> lenDelim
      PbBytes    -> lenDelim

    -- Getters
    getter = case ty of
      HsReq   t    -> qvar "Present" .<$>. getField t
      HsMaybe t    -> qvar "Just"    .<$>. getField t
      HsSeq t True -> getPacked t
      HsSeq t _    -> qvar "undefined" .<$>. getField t

    getPacked (HsBuiltin t) = app [ qvar "getPacked"
                                  , getPrim t
                                  ]
    getPacked _ = error "Impossible happened. Invalid packed option"

    getField (HsUserMessage _) = qvar "getMessage"
    getField (HsUserEnum    _) = qvar "getPbEnum"
    getField (HsBuiltin     t) = getPrim t

    getPrim t = case t of
      PbDouble   -> qvar "getFloat64le"
      PbFloat    -> qvar "getFloat32le"
      PbInt32    -> qvar "getVarInt32"
      PbInt64    -> qvar "getVarInt64"
      PbUInt32   -> qvar "getVarWord32"
      PbUInt64   -> qvar "getVarWord64"
      PbSInt32   -> qvar "getZigzag32"
      PbSInt64   -> qvar "getZigzag64"
      PbFixed32  -> qvar "getWord32le"
      PbFixed64  -> qvar "getWord64le"
      PbSFixed32 -> qvar "fromIntegral" .<$>. qvar "getWord32le"
      PbSFixed64 -> qvar "fromIntegral" .<$>. qvar "getWord64le"
      PbBool     -> qvar "getVarBool"
      PbString   -> qvar "getPbString"
      PbBytes    -> qvar "getPbBytestring"

----------------------------------------------------------------      

varint, fixed32, fixed64, lenDelim :: Integer
varint   = 0
fixed64  = 1
lenDelim = 2
fixed32  = 5

----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

modName :: [Identifier] -> ModuleName
modName = ModuleName . intercalate "." . map identifier


s :: SrcLoc
s =  SrcLoc "" 0 0


qname = Qual (ModuleName "P'") . Ident

var   = Var . UnQual . Ident
qvar  = Var . qname
con   = Con . UnQual . Ident
qcon  = Con . qname
tycon = TyCon . UnQual . Ident
pvar  = PVar . Ident
app = foldl1 App


f .<$>. g = app [ qvar "fmap" , f , g ]

instance_ cl ty decls =
  InstDecl s [] (qname cl) [ty] $ map InsDecl decls

fun  = (,)
bind = flip fun []
let_ xs e = Let (BDecls xs) e

p <-- e = Generator s p e

(name,pats) =: exp = FunBind [ Match s (Ident name) pats Nothing (UnGuardedRhs exp) (BDecls []) ]


class LiteralVal l where
  lit  :: l -> Exp
  plit :: l -> Pat

instance LiteralVal Integer   where 
  lit  = Lit  . Int
  plit = PLit . Int
instance LiteralVal String    where 
  lit  = Lit  . String
  plit = PLit . String
instance LiteralVal Bool      where 
  lit True  = Con $ qname "True"
  lit False = Con $ qname "False"
  plit = error "UNIMPLEMENTED"
  
instance LiteralVal Rational   where 
  lit  = Lit  . Frac
  plit = PLit . Frac

instance LiteralVal OptionVal where
  lit (OptString s) = lit s
  lit (OptBool   b) = lit b
  lit (OptInt    i) = lit i
  lit (OptReal   r) = lit r
  plit = error "UNIMPLEMENTED"
