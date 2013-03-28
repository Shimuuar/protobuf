{-# LANGUAGE TemplateHaskell #-}
-- |
-- Generation of instances using template haskell
module Data.Protobuf.TH (
    generateProtobuf
  ) where

import Control.Applicative
import Data.Int
import Data.Word
import Data.List       (intercalate)
import Data.ByteString (ByteString)
import Data.Sequence   (Seq)
import Language.Haskell.TH

import Data.Vector.HFixed (HVec)

import Data.Protobuf
import Data.Protobuf.API
import Data.Protobuf.Internal.AST hiding (Type,Message)



generateProtobuf :: [FilePath]  -- ^ Include path
                 -> [FilePath]  -- ^ Files to parse
                 -> Q [Dec]
generateProtobuf incs fnames = do
  -- Read protobuf files
  messages <- runIO $ loadProtobuf incs fnames
  -- Declare instance for every message
  case messages of
    Left  err -> fail err
    Right xs  -> concat <$> mapM genInstance xs

----------------------------------------------------------------
--
----------------------------------------------------------------

genInstance :: PbDatatype -> Q [Dec]
genInstance (PbMessage name fields) = do
  let tyFields = map findType fields
      fieldTypes = makeTyList $ map snd tyFields
  return
    -- Type instance for 'Message'
    [ TySynInstD ''Message [strLit name] $ ConT ''HVec `AppT` fieldTypes
    -- Type instance for 'FieldTypes'
    , TySynInstD ''FieldTypes [strLit name] $ fieldTypes
    -- Instance for 'Field' getter/setter
    -- Instance for 'Protobuf' (serialization/deserialization)  
    ]
genInstance (PbEnum name _) = do 
  return
    [ TySynInstD ''Message [strLit name] $ ConT ''Int ]


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
        TyMessage nm      -> ConT ''Msg `AppT` strLit nm
        TyEnum    nm      -> ConT ''Msg `AppT` strLit nm

----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

strLit :: QName -> Type
strLit = LitT . StrTyLit . unqualify

unqualify :: QName -> String
unqualify (QName ns n) = intercalate "." (ns ++ [n])

makeTyList :: [Type] -> Type
makeTyList = foldr (\a ls -> PromotedConsT `AppT` a `AppT` ls) PromotedNilT