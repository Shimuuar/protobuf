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

import Data.Vector.HFixed (HVec,Fun(..))

import Data.Protobuf
import Data.Protobuf.API
import Data.Protobuf.Internal.AST hiding (Type,Message,Field)



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

----------------------------------------------------------------
-- TH helpers
----------------------------------------------------------------

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
