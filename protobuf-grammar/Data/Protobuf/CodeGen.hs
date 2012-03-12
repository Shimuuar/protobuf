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
import Data.Generics.Uniplate.Data

import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.Syntax
import Debug.Trace

-- | Convert module into the haskell code to be dumped
convert :: ([Identifier TagType],HsModule) -> Module
convert (qs, msg) =
  Module s (modName qs)
   [ LanguagePragma s [ Ident "DeriveDataTypeable"
                      , Ident "NoImplicitPrelude"
                      , Ident "FlexibleInstances"
                      , Ident "KindSignatures"
                      , Ident "StandaloneDeriving"
                      ] ]
   Nothing Nothing
   -- Imports
   ( ImportDecl { importLoc       = s
                , importModule    = ModuleName "Data.Protobuf.Imports"
                , importQualified = True
                , importSrc       = False
                , importPkg       = Nothing
                , importAs        = Just $ ModuleName "P'"
                , importSpecs     = Nothing
                } 
     : importList msg
   )
   -- Code
   (convertDecl msg)

importList :: HsModule -> [ImportDecl]
importList = map toImport . concatMap pick . universeBi
  where
    pick (HsBuiltin _)                    = []
    pick (HsUserMessage (Qualified qs q)) = [qs ++ [q]]
    pick (HsUserEnum    (Qualified qs q)) = [qs ++ [q]]
    --
    toImport qs = ImportDecl { importLoc       = s
                             , importModule    = ModuleName $ intercalate "." $ map identifier qs
                             , importQualified = True
                             , importSrc       = False
                             , importPkg       = Nothing
                             , importAs        = Nothing
                             , importSpecs     = Nothing
                             }

-- | Convert declaration
convertDecl :: HsModule -> [Decl]
convertDecl (HsMessage (TyName name) fields) =
  -- Data declaration
  [ DataDecl s DataType [] (Ident name) [KindedVar (Ident "r") (KindStar `KindFn` KindStar)]
      [ QualConDecl s [] [] $ RecDecl (Ident name) (map recordField fields)
      ]
      []
  , DerivDecl s [] (qname "Show") [ tycon name `TyApp` qtycon "Required" ]
  , DerivDecl s [] (qname "Show") [ tycon name `TyApp` qtycon "Val" ]
  , instance_ "Default" (tycon name `TyApp` qtycon "Required")
      [ bind "def" =: foldl App (con name)
          [ case defV of
              Just v  -> lit v
              Nothing -> qvar "def"
          | HsField _ _ _ defV <- fields ]
      ]
  , instance_ "MessageField" (tycon name `TyApp` qtycon "Required")
      [ let ns1 = patNames "x" fields
            ns2 = patNames "y" fields
        in fun "mergeField" [ (PApp $ UnQual $ Ident name) (map PVar ns1)
                            , (PApp $ UnQual $ Ident name) (map PVar ns2)
                            ]
              =: appF (con name)
                      [ app [ qvar "mergeField"
                            , Var (UnQual n1)
                            , Var (UnQual n2)
                            ]
                      | (n1, n2) <- zip ns1 ns2
                      ]
      ]
  , instance_ "Monoid" (tycon name `TyApp` qtycon "Required")
      [ bind "mempty"  =: qvar "def"
      , bind "mappend" =: qvar "mergeField"
      ]
  , instance_ "Message" (tycon name)
      [ bind "getMessage" =:
          let_ [ TypeSig s [Ident "loop"] (qtycon "LoopType" `TyApp` tycon name)
               , bind "loop" =:
                   Lambda s [pvar "v"]
                   ( Do [ pvar "done" <-- qvar "isEmpty"
                        , Qualifier $ If (var "done") 
                            (app [ qvar "return" , var "v" ])
                            (Do [ pvar "wt" <-- qvar "getWireTag"
                                , Qualifier $
                                  Case (var "wt") $ 
                                   concat [ caseField (length fields) i f | (i,f) <- enum fields]
                                   ++ [ Alt s PWildCard 
                                        (UnGuardedAlt $
                                         Do [ Qualifier $ app [ qvar "skipUnknownField" 
                                                              , var  "wt" ]
                                            , Qualifier $ app [ var "loop"
                                                              , var "v" ]
                                            ]
                                        )
                                        (BDecls [])
                                      ]
                                ]
                            )
                        ]
                   )
               ]
          (app [ var "loop"
               , qvar "mempty"
               ] )
      , checkReq name fields
      , bind "putMessage" =: qvar "undefined"
      ]
  ]
convertDecl (HsEnum    (TyName name) fields) =
  -- Data declaration
  [ DataDecl s DataType [] (Ident name) []
      -- Constructors
      [ QualConDecl s [] [] (ConDecl (Ident n) []) | (TyName n, _) <- fields ]
      -- Deriving clause
      [ (qname "Show", []), (qname "Eq", []) ]
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
  , instance_ "MessageField" (tycon name) []
  ]

derives = map (\n -> (qname n, [])) []
  -- [ "Show", "Eq" ]

checkReq name fields =
  fun "checkReq"  [PApp (UnQual (Ident name)) (map PVar ns)] =:
    foldl (\e1 e2 -> app [ qvar "ap", e1, e2]) 
      (app [ qvar "return", var name ])
      (zipWith check ns fields)
  where
    ns = patNames "x" fields
    -- Repeated
    check n (HsField (HsSeq (HsBuiltin _) _) _ _ _)        
      = app [ qvar "return", Var $ UnQual n ]
    check n (HsField (HsSeq (HsUserEnum _) _) _ _ _)        
      = app [ qvar "return", Var $ UnQual n ]
    check n (HsField (HsSeq (HsUserMessage _) _) _ _ _)        
      = app [ qvar "mapM", qvar "checkReq", Var $ UnQual n ]
    -- Optional
    check n (HsField (HsMaybe (HsBuiltin _)) _ _ (Just o)) 
      = app [ qvar "checkMaybe", lit o, Var (UnQual n)]
    check n (HsField (HsMaybe (HsUserEnum _)) _ _ (Just o)) 
      = app [ qvar "checkMaybe", lit o, Var (UnQual n)]
        
    check n (HsField (HsMaybe (HsBuiltin _)) _ _ _)
      = app [ qvar "return", Var $ UnQual n ]      
    check n (HsField (HsMaybe (HsUserEnum _)) _ _ _)
      = app [ qvar "return", Var $ UnQual n ]

        
    check n (HsField (HsMaybe _) _ _ _)
      = app [ qvar "checkMaybeMsg"
            , Var (UnQual n)]
    -- Required
    check n (HsField (HsReq (HsBuiltin _)) _ _ _)        
      = app [ qvar "checkRequired"  , Var (UnQual n) ]
    check n (HsField (HsReq (HsUserEnum _)) _ _ _)        
      = app [ qvar "checkRequired"  , Var (UnQual n) ]
    check n (HsField (HsReq (HsUserMessage _)) _ _ _)        
      = app [ qvar "checkRequiredMsg"  , Var (UnQual n) ]
    
-- | Single fields of record
recordField :: HsField -> ([Name], BangType)
recordField (HsField tp name _ _) =
  ([Ident name], outerType tp)
  where
    outerType (HsReq   t  ) = BangedTy $ TyVar (Ident "r")     `TyApp` innerType t
    outerType (HsMaybe t  ) = BangedTy $ TyCon (qname "Maybe") `TyApp` innerType t
    outerType (HsSeq   t _) = BangedTy $ TyCon (qname "Seq"  ) `TyApp` innerType t

    innerType (HsBuiltin     t) = primType t
    innerType (HsUserMessage q) = userType q
    innerType (HsUserEnum    q) = enumType q

    userType (Qualified qs n) = 
      (TyCon $ Qual (modName (qs++[n])) (Ident $ identifier n)) `TyApp` TyVar (Ident "r")
    enumType (Qualified qs n) =
      (TyCon $ Qual (modName (qs++[n])) (Ident $ identifier n))
      
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
    primType PbBytes    = TyCon $ qname "ByteString"

    sint32 = TyCon $ qname "Int32"
    sint64 = TyCon $ qname "Int64"
    uint32 = TyCon $ qname "Word32"
    uint64 = TyCon $ qname "Word64"


caseField n i (HsField ty name (FieldTag tag) _) =
  -- We have found tag
  [ Alt s (PApp (qname "WireTag") [plit tag, plit typeTag])
    (UnGuardedAlt $
     Do [ pvar "f" <-- getter
        , Qualifier $ app [ var "loop"
                          , RecUpdate (var "v") [
                            FieldUpdate (UnQual $ Ident name)
                              (app [ qvar "mergeField"
                                   , app [ var name
                                         , var "v"
                                         ]
                                   , var "f"
                                   ]
                              )
                            ]
                          ]
        ]
    )
    (BDecls [])
  -- Oops! wrong field type
  , Alt s (PApp (qname "WireTag") [plit tag, pvar "zzz"])
    (UnGuardedAlt $ app [ qvar "fail"
                        , app [ qvar "mconcat"
                              , List 
                                [ lit "Invalid tag! "
                                , app [ qvar "show" , var "zzz" ]
                                , lit " expected "
                                , lit (show typeTag)
                                , lit (" ["++name++"]")
                                , lit ("tag="++show tag++" " )
                                , lit $ show ty
                                ]
                              ]
                        ]
    )
    (BDecls [])
  ]
  where
    pnames = patNames "f" [1..n]
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
      HsSeq t _    -> qvar "singleton" .<$>. getField t

    getPacked (HsBuiltin t) = app [ qvar "label"
                                  , lit $  name ++ "[packed]"
                                  , app [ qvar "getPacked"
                                        , getPrim t
                                        ]
                                  ]
    getPacked _ = error "Impossible happened. Invalid packed option"

    getField (HsUserMessage _) = app [ qvar "label" 
                                     , lit  name
                                     , qvar "getDelimMessage"
                                     ]
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

modName :: [Identifier TagType] -> ModuleName
modName = ModuleName . intercalate "." . map identifier


s :: SrcLoc
s =  SrcLoc "" 0 0


qname = Qual (ModuleName "P'") . Ident

var    = Var . UnQual . Ident
qvar   = Var . qname
con    = Con . UnQual . Ident
qcon   = Con . qname
tycon  = TyCon . UnQual . Ident
qtycon = TyCon . qname
pvar   = PVar . Ident
app    = foldl1 App
appF   = foldl  App

f .<$>. g = app [ qvar "fmap" , f , g ]

instance_ cl ty decls =
  InstDecl s [] (qname cl) [ty] $ map InsDecl decls

fun  = (,)
bind = flip fun []
let_ xs e = Let (BDecls xs) e

p <-- e = Generator s p e

(name,pats) =: exp = FunBind [ Match s (Ident name) pats Nothing (UnGuardedRhs exp) (BDecls []) ]

patNames pref xs = [ Ident $ pref ++ show i | (i,_) <- zip [1..] xs ]

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

enum = zip [0..]