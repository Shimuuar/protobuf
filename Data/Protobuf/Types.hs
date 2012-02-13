{-# LANGUAGE DeriveFunctor #-}
-- | Data types for transformations
module Data.Protobuf.Types where
  --   PbMonad
  -- , askContext
  -- , PbContext(..)
  -- , FileMap
  -- ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Error

import Data.Map   (Map)
import Data.Set   (Set)
import Data.Ord
import Data.Function

import Data.Protobuf.AST


----------------------------------------------------------------
-- Auxillary data types & synonims
----------------------------------------------------------------

-- | Set of all protobuf files
data Bundle a = Bundle
  { importMap  :: Map String FilePath
    -- ^ Maps import strings to the pathes in the file system
  , packageMap :: Map FilePath (PbFile a)
    -- ^ Map file pathes to packages.
  }

-- | Protobuf file data
data PbFile a = PbFile [Protobuf] a
                deriving Functor

-- | Package namespace. Either a package namespace or 
data Namespace 
  = PackageName  Identifier  Namespace
  | TopLevel     (Set SomeName)


-- | Single name in a set
data SomeName
  = MsgName   Identifier (Set SomeName)
  | FieldName Identifier
  | EnumName  Identifier
  | EnumElem  Identifier

nameToId :: SomeName -> Identifier
nameToId (MsgName   n _) = n
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

