{-# LANGUAGE DeriveDataTypeable #-}
-- |
module Data.Protobuf (
    -- * Data types
    QName(..)
  , PbDatatype(..)
  , PbField(..)
  , PbType(..)
  , PbOption(..)
  , OptionVal(..)
    -- * Reading of protobuf
  , loadProtobuf
  ) where

import Data.Data (Data,Typeable)
import qualified Data.Foldable    as F
import qualified Data.Traversable as T
import Data.Generics.Uniplate.Data

import Data.Protobuf.Internal.AST
import Data.Protobuf.Internal.Names
import Data.Protobuf.Internal.Control
import Data.Protobuf.Internal.Types
import Data.Protobuf.Internal.Transform



----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Qualified name
data QName = QName [String] String
           deriving (Show,Eq,Typeable,Data)

-- | Data type declared in the .proto file. It could be either message
--   or enumeration.
data PbDatatype
  = PbMessage QName [PbField]
  | PbEnum    QName [(Integer,String)]
  deriving (Show,Eq,Typeable,Data)

-- | Field of message.
data PbField = PbField Modifier PbType String Integer [PbOption]
             deriving (Show,Eq,Typeable,Data)

-- | Type of field
data PbType
  = TyMessage QName
  | TyEnum    QName
  | TyPrim    PrimType
  deriving (Show,Eq,Typeable,Data)

-- | Supported protobuf options
data PbOption
  = OptDefault OptionVal
  | OptRepeated
  deriving (Show,Eq,Typeable,Data)



----------------------------------------------------------------
-- Reading protobuf source
----------------------------------------------------------------

-- | Load all protobuf files
loadProtobuf :: [String]                   -- ^ Search path for includes
             -> [FilePath]                 -- ^ Files to load
             -> IO (Either String [PbDatatype])
loadProtobuf includes srcs = runPbMonad (PbContext includes) $ do
  -- 1. Actually load files
  bundle <- loadPbFiles srcs
  -- 2. Check sources for obvious error
  F.mapM_ checkLabels bundle
  -- 3. Build namespaces
  Bundle pathes dmap <- T.traverse buildNamespace
                      $ fmap sortLabels bundle
  let toPB path = case lookupDMap2 path dmap of
                    Just x  -> x
                    Nothing -> error "Internal error in protobuf"
      files = map toPB pathes
  -- 4. Result
  res <-  mapM resolveTypeNames
      =<< mapM (mergeImports dmap) files
  -- Extract data from AST
  return $ extractData =<< res


extractData :: ProtobufFile -> [PbDatatype]
extractData pb =
  [ cnvMessage x | x <- universeBi $ protobufFile pb ] ++
  [ cnvEnum    x | x <- universeBi $ protobufFile pb ]
  where
    -- Extract messages
    cnvMessage (Message nm fields path)
      = PbMessage (makeQN path nm)
                  (cnvField =<< fields)
    cnvField (MessageField (Field modif ty name (FieldTag tag) _))
      = [PbField modif fType (identifier name) tag []]
      where
        fType = case ty of
                  BaseType p -> TyPrim p
                  EnumType q -> TyEnum    $ qname q
                  MsgType  q -> TyMessage $ qname q
                  _          -> error "impossible 21"
    cnvField _ = []
    -- Extract enums
    cnvEnum (EnumDecl nm fields path)
      = PbEnum (makeQN path nm)
        [ (i,name) | EnumField (Identifier name) i <- fields]
    -- names
    makeQN path nm = QName (map identifier $ init path) (identifier nm)
    --
    qname (FullQualId (Qualified path nm)) = makeQN path nm
    qname _ = error "Impossible 22"
