{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
-- | Data types for transformations
module Data.Protobuf.Types (
    -- * 
  --   Bundle(..)
  -- , applyBundle
  -- , applyBundleM
  -- , applyBundleM_
    -- * Namespaces
    -- * Monads
    PbMonad
  , runPbMonad
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
import qualified Data.Map         as Map
import           Data.Map           (Map)
import qualified Data.Foldable    as F
import qualified Data.Traversable as T

import Data.Ord
import Data.Function

-- import Data.Protobuf.AST



----------------------------------------------------------------
-- Auxillary data types & synonims
----------------------------------------------------------------

-- -- | Set of all protobuf files to be processed
-- data Bundle n = Bundle
--   { processedFiles :: [FilePath]
--     -- ^ Files to be processed
--   , importMap  :: Map String FilePath
--     -- ^ Maps import strings to the pathes in the file system
--   , packageMap :: Map FilePath (ProtobufFile n)
--     -- ^ Map file pathes to packages.
--   }
--   deriving (Data,Typeable)

-- applyBundle :: (ProtobufFile a -> ProtobufFile b) -> Bundle a -> Bundle b
-- applyBundle f (Bundle ps imap pmap) =
--   Bundle ps imap (fmap f pmap)

-- applyBundleM_ :: Monad m => (ProtobufFile a -> m ()) -> Bundle a -> m ()
-- applyBundleM_ f (Bundle _ _ pmap) =
--   F.mapM_ f pmap

-- applyBundleM :: Monad m => (ProtobufFile a -> m (ProtobufFile b)) -> Bundle a -> m (Bundle b)
-- applyBundleM f (Bundle ps imap pmap) =
--   Bundle ps imap `liftM` T.mapM f pmap




----------------------------------------------------------------
-- Monads
----------------------------------------------------------------

-- | Context for protobuf
data PbContext = PbContext
  { includePaths :: [String]
    -- ^ List of include path
  }

-- | Monad for working with protobuf source tree.
type PbMonad =
  ErrorT String
   (ReaderT PbContext IO)

-- | Execute PbMonad
runPbMonad :: PbContext -> PbMonad a -> IO (Either String a)
runPbMonad cxt
  = flip runReaderT cxt
  . runErrorT



-- | Monad which allows to accumulate non-fatal errors
type PbMonadE =
  WriterT [String]
    PbMonad

-- | Non-fatal error
oops :: String -> PbMonadE()
oops = tell . (:[])

-- | Collect all non-fatal errors. If there are any raise an error.
collectErrors :: PbMonadE a -> PbMonad a
collectErrors m = do
  (x,errs) <- runWriterT m
  case errs of
    [] -> return x
    _  -> throwError $ unlines errs

-- | Ask for context
askContext :: PbMonad PbContext
askContext = lift ask
