{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
-- | Data types for transformations
module Data.Protobuf.Types (
    -- * 
    Bundle(..)
    -- *
  , Qualified(..)
  , addQualifier
  , addQualList
  , SomeName(..)
  , Namespace
  , packageNamespace
  , emptyNamespace
  , findName
  , findQualName
  , insertName
  , mergeNamespaces
    -- *
  , Names(..)
  , resolveName
  , nameDown
    -- *
  , PbMonad
  , PbMonadE
  , oops
  , collectErrors
  , askContext
  , PbContext(..)
  ) where


import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Error

import Data.Data                 (Typeable,Data)
import Data.Functor
import qualified Data.Map      as Map
import           Data.Map        (Map)
import qualified Data.Foldable as F

import Data.Set   (Set)
import Data.Ord
import Data.Function

import Data.Protobuf.AST


----------------------------------------------------------------
-- Auxillary data types & synonims
----------------------------------------------------------------

-- | Set of all protobuf files to be processed
data Bundle n = Bundle
  { processedFiles :: [FilePath]
    -- ^ Files to be processed
  , importMap  :: Map String FilePath
    -- ^ Maps import strings to the pathes in the file system
  , packageMap :: Map FilePath (ProtobufFile n)
    -- ^ Map file pathes to packages.
  }



----------------------------------------------------------------
-- Namespace management
----------------------------------------------------------------


-- | Single name in a set
data SomeName
  = MsgName   Identifier Namespace
  | PkgName   Identifier Namespace
  | FieldName Identifier
  | EnumName  Identifier
  | EnumElem  Identifier
  deriving (Show,Typeable,Data)



----------------------------------------

-- | Namespace
newtype Namespace = Namespace (Map Identifier SomeName)
                  deriving (Typeable,Data)

instance Show Namespace where
  show (Namespace m) = show $ Map.elems m
                           
-- | Empty namespace
emptyNamespace :: Namespace
emptyNamespace = Namespace Map.empty

-- | Put namespace into package
packageNamespace :: Identifier -> Namespace -> Namespace
packageNamespace pkg ns = 
  Namespace $ Map.singleton pkg (PkgName pkg ns)

-- | Find name in namespace
findName :: Namespace -> Identifier -> Maybe SomeName
findName (Namespace ns) n = Map.lookup n ns

-- | Find qualified name in the namespace
findQualName :: Namespace -> Qualified Identifier -> Maybe (Qualified SomeName)
findQualName names (Qualified [] n)
  = Qualified [] <$> findName names n
findQualName names (Qualified (q:qs) n) = do
  nest <- findName names q
  case nest of
    MsgName _ ns -> addQualifier q <$> findQualName ns (Qualified qs n)
    PkgName _ ns -> addQualifier q <$> findQualName ns (Qualified qs n)
    _            -> Nothing

-- | Insert name into set while checking for duplicates
insertName :: Namespace -> SomeName -> PbMonadE (Namespace)
insertName (Namespace ns) name
  | n `Map.member` ns = do oops $ "Duplicate name: " ++ identifier n
                           return (Namespace ns)
  | otherwise         = return $ Namespace $ Map.insert n name ns
  where
    n = nameToId name

-- | Merge namespaces and collect error during that
mergeNamespaces :: Namespace -> Namespace -> PbMonadE Namespace
mergeNamespaces namespace (Namespace ns2)
  = F.foldlM insertName namespace ns2


nameToId :: SomeName -> Identifier
nameToId (MsgName   n _) = n
nameToId (PkgName   n _) = n
nameToId (FieldName n  ) = n
nameToId (EnumName  n  ) = n
nameToId (EnumElem  n  ) = n

instance Eq SomeName where
  (==) = (==) `on` nameToId
instance Ord SomeName where
  compare = comparing nameToId



----------------------------------------

-- | Set of namespaces. First parameter is global namespace and second
--   is current path into namespace
data Names = Names Namespace [Identifier]
           deriving (Show,Typeable,Data)

nameDown n (Names global path) = Names global (path ++ [n])

resolveName :: Names -> QIdentifier -> PbMonadE (Qualified SomeName)
resolveName (Names global _ ) (FullQualId qs n) = resolveNameWorker global qs n
resolveName (Names global []) (QualId     qs n) = resolveNameWorker global qs n
resolveName (Names global path) name@(QualId qs n) =
  case findQualName global (Qualified (path ++ qs) n) of
    Just x  -> return x
    Nothing -> resolveName (Names global (init path)) name

resolveNameWorker namespace qs n =
  case findQualName namespace (Qualified qs n) of
    Just x  -> return x
    Nothing -> oops "Cannot find name" >> return (error "Impossible")  



----------------------------------------------------------------
-- Monads
----------------------------------------------------------------

-- | Context for protobuf
data PbContext = PbContext { includePaths :: [String]
                           }

-- | Monad for reading and transformations
type PbMonad =
  ErrorT String
   (ReaderT PbContext IO)

-- | Monad which allows to accumulate non-fatal errors
type PbMonadE =
  WriterT [String]
    PbMonad

-- | Non-fatal error
oops :: String -> PbMonadE()
oops = tell . (:[])

collectErrors :: PbMonadE a -> PbMonad a
collectErrors m = do
  (x,errs) <- runWriterT m
  case errs of
    [] -> return x
    _  -> throwError $ unlines errs

-- | Ask for context
askContext :: PbMonad PbContext
askContext = lift ask

