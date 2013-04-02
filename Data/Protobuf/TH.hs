{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Generation of instances using template haskell
module Data.Protobuf.TH (
    generateProtobuf
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Data.Int
import Data.Word
import Data.List       (intercalate)
import Data.ByteString (ByteString)
import Data.Sequence   (Seq)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (qAddDependentFile)
import GHC.TypeLits

import Data.Vector.HFixed (HVec,Fun(..),newMutableHVec)

import Data.Protobuf
import Data.Protobuf.API
import Data.Protobuf.Internal.AST hiding (Type,Message,Field,Protobuf)
import Data.Protobuf.Serialize.Protobuf
import Data.Protobuf.Serialize.VarInt
import Data.Serialize.IEEE754



generateProtobuf :: [FilePath]  -- ^ Include path
                 -> [FilePath]  -- ^ Files to parse
                 -> Q [Dec]
generateProtobuf incs fnames = do
  -- Read protobuf files
  messages <- runIO $ loadProtobuf incs fnames
  mapM_ qAddDependentFile fnames
  -- Declare instance for every message
  case messages of
    Left  err -> fail err
    Right xs  -> concat <$> mapM genInstance xs

----------------------------------------------------------------
-- Workers
----------------------------------------------------------------

-- Generate instance for the data type
genInstance :: PbDatatype -> Q [Dec]
genInstance (PbMessage name fields) = do
  let tyFields   = map findType fields
      fieldTypes = makeTyList $ map snd tyFields
  execWriterT $ do
    -- Type instance for 'Message'
    tell [ TySynInstD ''Message [qstrLit name] $ ConT ''HVec `AppT` fieldTypes ]
    -- Type instance for 'FieldTypes'
    tell [ TySynInstD ''FieldTypes [qstrLit name] $ fieldTypes ]
    -- Instance for 'Field' getter/setter
    forM (zip [0..] tyFields) $ \(i,(field,ty)) -> do
      lam <- lift $ getterTH (length tyFields) i
      tell [ InstanceD [] (ConT ''Field `AppT` qstrLit name `AppT` strLit field)
               [ TySynInstD ''FieldTy [qstrLit name, strLit field] ty
               , ValD (VarP 'getterF) (NormalB $ AppE (ConE 'Fun) lam) []
               ]
           ]
    -- Instance for 'Protobuf' (serialization/deserialization)
    deser <- lift $ deserializeDecl name fields
    tell [ InstanceD [] (ConT ''Protobuf `AppT` (qstrLit name))
             [ ValD (VarP 'serialize)    (NormalB $ VarE 'undefined) []
             , ValD (VarP 'getMessageST) (NormalB $ deser) []
             ]
         ]
genInstance (PbEnum name _) = do 
  return
    [ TySynInstD ''Message [qstrLit name] $ ConT ''Int ]


-- Produce pair (name, type) for field of the message.
findType :: PbField -> (String, Type)
findType (PbField m ty name _)
  = (name, modifyTy baseTy)
  where
    modifyTy =
      case  m of
        Required -> id
        Optional -> AppT $ ConT ''Maybe
        Repeated -> AppT $ ConT ''Seq
    -- Basic type where (required/optional/repeated) modifiers are not
    -- applied
    baseTy =
      case ty of
        -- Primitive types
        TyPrim PbDouble   -> ConT ''Double
        TyPrim PbFloat    -> ConT ''Float
        TyPrim PbInt32    -> ConT ''Int32
        TyPrim PbInt64    -> ConT ''Int64
        TyPrim PbUInt32   -> ConT ''Word32
        TyPrim PbUInt64   -> ConT ''Word64
        TyPrim PbSInt32   -> ConT ''Int32
        TyPrim PbSInt64   -> ConT ''Int64
        TyPrim PbFixed32  -> ConT ''Int32
        TyPrim PbFixed64  -> ConT ''Int64
        TyPrim PbSFixed32 -> ConT ''Int32
        TyPrim PbSFixed64 -> ConT ''Int64
        TyPrim PbBool     -> ConT ''Bool
        TyPrim PbString   -> ConT ''String
        TyPrim PbBytes    -> ConT ''ByteString
        -- Custom types
        TyMessage nm      -> ConT ''Msg `AppT` qstrLit nm
        TyEnum    nm      -> ConT ''Msg `AppT` qstrLit nm

-- Declaration of deserialization function. General layout
--
-- > getRecords updFun emptyRec
deserializeDecl name fields = do
  updFun <- newName "updFun"
  emp    <- newName "emp"
  --
  letE [ valD (varP emp)
           (normalB $ (conE 'MutableMsg `appE` varE 'newMutableHVec `appE` conE '())
                    `sigE`
                      (conT ''MutableMsg `appT` return (qstrLit name))
           )
         []
       , updateDecl updFun (zip [0..] fields)
       ] $
      (varE 'getRecords `appE` varE updFun `appE` varE emp)

-- Function for updating single record
updateDecl funNm fields = do
  -- Primary clauses
  cls <- mapM updateClause fields
  -- Fallback clause
  fallback <- do
    wt  <- newName "wt"
    msg <- newName "msg"
    clause [varP wt, varP msg]
      (normalB $ doE [ noBindS $ varE 'skipUnknownField `appE` varE wt
                     , noBindS $ varE 'return           `appE` varE msg
                     ]
      )
      []
  --
  return $ FunD funNm (concat cls ++ [fallback])

-- Update clause for single field
updateClause (i,(PbField modif ty _ tag)) = do
  msg <- newName "msg"
  --
  let updater = case modif of
                  Required -> varE 'writeRequired
                  Optional -> varE 'writeOptional
                  Repeated -> varE 'writeRepeated
      n       = varE 'sing `sigE` (conT ''Sing `appT` litT (numTyLit i))
      updExpr = updater `appE` n `appE` varE (fieldParser ty) `appE` varE msg
  --
  sequence
    [ clause
        [ conP 'WireTag [litP (IntegerL tag), litP (IntegerL $ fromIntegral $ getTyTag ty)]
        , varP msg
        ]
        (normalB $ updExpr)
         []
    , clause
        [ conP 'WireTag [litP (IntegerL tag), wildP]
        , wildP
        ]
        (normalB $ varE 'fail `appE` litE (StringL "Bad wire tag"))
        []
    ]

----------------------------------------------------------------
-- TH helpers
----------------------------------------------------------------

getTyTag (TyPrim PbDouble)   = tag_FIXED64
getTyTag (TyPrim PbFloat)    = tag_FIXED32
getTyTag (TyPrim PbInt32)    = tag_VARINT
getTyTag (TyPrim PbInt64)    = tag_VARINT
getTyTag (TyPrim PbUInt32)   = tag_VARINT
getTyTag (TyPrim PbUInt64)   = tag_VARINT
getTyTag (TyPrim PbSInt32)   = tag_VARINT
getTyTag (TyPrim PbSInt64)   = tag_VARINT
getTyTag (TyPrim PbFixed32)  = tag_FIXED32
getTyTag (TyPrim PbFixed64)  = tag_FIXED64
getTyTag (TyPrim PbSFixed32) = tag_FIXED32
getTyTag (TyPrim PbSFixed64) = tag_FIXED64
getTyTag (TyPrim PbBool)     = tag_VARINT
getTyTag (TyPrim PbString)   = tag_LENDELIM
getTyTag (TyPrim PbBytes)    = tag_LENDELIM
        -- Custom types
getTyTag (TyMessage nm)      = tag_LENDELIM
getTyTag (TyEnum    nm)      = tag_VARINT

fieldParser (TyPrim PbDouble)   = 'getFloat64le
fieldParser (TyPrim PbFloat)    = 'getFloat32le
fieldParser (TyPrim PbInt32)    = 'getVarInt32
fieldParser (TyPrim PbInt64)    = 'getVarInt64
fieldParser (TyPrim PbUInt32)   = 'getVarWord32
fieldParser (TyPrim PbUInt64)   = 'getVarWord64
fieldParser (TyPrim PbSInt32)   = 'getZigzag32
fieldParser (TyPrim PbSInt64)   = 'getZigzag64
fieldParser (TyPrim PbFixed32)  = undefined
fieldParser (TyPrim PbFixed64)  = undefined
fieldParser (TyPrim PbSFixed32) = undefined
fieldParser (TyPrim PbSFixed64) = undefined
fieldParser (TyPrim PbBool)     = 'getVarBool
fieldParser (TyPrim PbString)   = 'getPbString
fieldParser (TyPrim PbBytes)    = 'getPbBytestring
        -- Custom types
fieldParser (TyMessage nm)      = undefined
fieldParser (TyEnum    nm)      = undefined

    
strLit :: String -> Type
strLit = LitT . StrTyLit

qstrLit :: QName -> Type
qstrLit = strLit . unqualify

unqualify :: QName -> String
unqualify (QName ns n) = intercalate "." (ns ++ [n])

makeTyList :: [Type] -> Type
makeTyList = foldr (\a ls -> PromotedConsT `AppT` a `AppT` ls) PromotedNilT


getterTH :: Int -> Int -> Q Exp
getterTH nTot n = do
  nm <- newName "x"
  lamE
    [ if i == n then varP nm else wildP | i <- take nTot [0..]]
    (varE nm)
