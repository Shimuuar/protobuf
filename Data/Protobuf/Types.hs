{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
-- | Data types for transformations
module Data.Protobuf.Types (
    -- * Monads
    PbMonad
  , runPbMonad
  , PbMonadE
  , oops
  , collectErrors
  , askContext
  , PbContext(..)
    -- * Double map
  , DMap
  , emptyDMap
  , lookupDMap
  , insertDMapM
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


----------------------------------------------------------------
-- Double map
----------------------------------------------------------------

-- | Map which maps first set of key to second one. This is needed
--   either to preserve sharing or when first set of keys could be
--   larger than second and wee want to avoid keeping duplicates.
data DMap k1 k2 v = DMap (Map k1 k2) (Map k2 v)
                    deriving (Typeable,Functor)

-- | Empty map
emptyDMap :: DMap k1 k2 v
emptyDMap = DMap Map.empty Map.empty

-- | Lookup value in the map.
lookupDMap :: (Ord k1, Ord k2) => k1 -> DMap k1 k2 v -> Maybe v
lookupDMap k1 (DMap m1 m2) =
  case k1 `Map.lookup` m1 of
    Nothing -> Nothing
    Just k2 -> case k2 `Map.lookup` m2 of
                 Nothing -> error "Internal error in Data.Protobuf.Types.DMap"
                 r       -> r

-- | Insert value into double map using monadic action. 
insertDMapM :: (Ord k1, Ord k2, Monad m)
            => (k1 -> m k2)     -- ^ Mapping from first key to second
            -> (k2 -> m v)      -- ^ Mapping from second key to value
            -> k1               -- ^ Key to insert
            -> DMap k1 k2 v
            -> m (Maybe v,DMap k1 k2 v)
insertDMapM fKey fVal k1 m@(DMap m1 m2)
  | k1 `Map.member` m1 = return (Nothing,m)
  | otherwise          = do
      k2 <- fKey k1
      if k2 `Map.member` m2
        then return (Nothing, DMap (Map.insert k1 k2 m1) m2)
        else do v <- fVal k2
                return ( Just v
                       , DMap (Map.insert k1 k2 m1)
                              (Map.insert k2 v  m2)
                       )
