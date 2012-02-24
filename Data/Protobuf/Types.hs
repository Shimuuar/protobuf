{-# LANGUAGE DeriveFunctor #-}
-- | Data types for transformations
module Data.Protobuf.Types (
    -- * 
    Bundle(..)
  , PbFile(..)
    -- *
  , Qualified(..)
  , addQualifier
  , SomeName(..)
  , Namespace
  , packageNamespace
  , emptyNamespace
  , findName
  , findQualName
  , insertName
  , mergeNamespaces
    -- *
  , PbMonad
  , PbMonadE
  , oops
  , collectErrors
  , askContext
  , PbContext(..)
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Error

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
data Bundle a = Bundle
  { processedFiles :: [FilePath]
    -- ^ Files to be processed
  , importMap  :: Map String FilePath
    -- ^ Maps import strings to the pathes in the file system
  , packageMap :: Map FilePath (PbFile a)
    -- ^ Map file pathes to packages.
  }
  deriving (Functor)


-- | Protobuf file data
data PbFile a = PbFile [Protobuf] a
                deriving Functor



----------------------------------------------------------------
-- Namespace management
----------------------------------------------------------------

-- | Qualified name
data Qualified a = Qualified [Identifier] a

addQualifier :: Identifier -> Qualified a -> Qualified a
addQualifier q (Qualified qs x) = Qualified (q:qs) x

-- | Single name in a set
data SomeName
  = MsgName   Identifier Namespace
  | PkgName   Identifier Namespace
  | FieldName Identifier
  | EnumName  Identifier
  | EnumElem  Identifier
  deriving (Show)

-- | Namespace
newtype Namespace = Namespace (Map Identifier SomeName)
                    deriving Show

-- | Empty namespace
emptyNamespace :: Namespace
emptyNamespace = Namespace Map.empty

packageNamespace :: Identifier -> Namespace -> Namespace
packageNamespace pkg ns = 
  Namespace $ Map.singleton pkg (PkgName pkg ns)

-- | Find name in namespace
findName :: Namespace -> Identifier -> Maybe SomeName
findName (Namespace ns) n = Map.lookup n ns

-- | Find 
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

