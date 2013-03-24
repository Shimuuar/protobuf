-- |
module Data.Protobuf where

import Data.Serialize       (Get)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Foldable    as F
import qualified Data.Traversable as T
import Data.Generics.Uniplate.Data

import Data.Protobuf.Internal.AST
import Data.Protobuf.Internal.Types
import Data.Protobuf.Internal.Transform




----------------------------------------------------------------
-- Reading protobuf source
----------------------------------------------------------------

-- | Qualified name
data QName = QName [String] String

data PbDatatype
  = PbMessage QName [PbField]
  | PbEnum    QName [(Integer,String)]

data PbField = PbField Modifier QName String Integer

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
  
  undefined

{-
extractData :: ProtobufFile -> [PbDatatype]
extractData pb =
  [ cnvMessage x | x <- universeBi pb ] ++
  [ cnvEnum    x | x <- universeBi pb ]
  where
    cnvMessage (Message nm fields path)
      = PbMessage (QName (map identifier path) (identifier nm))
                  (cnvField =<< fields)
    cnvField (MessageField (Field mod ty (Identifier fld) _ _)) = []
    cnvField _ = []
    cnvEnum = undefined
-}