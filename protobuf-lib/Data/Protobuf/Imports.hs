-- | This module reexport everything which is needed for message
--   definition in generated code. There is no need import this module
--   in user code.
module Data.Protobuf.Imports (
    -- * Data types
    Word32
  , Word64
  , Int32
  , Int64
  , Bool
  , Double
  , Float
  , String
  , Maybe(..)
  , Seq
  , ByteString
    -- * Functions
  , singleton
  , undefined
  , comparing    
  , ap
  , mapM
  , mapM_
    -- ** Converwsion
  , checkMaybe
  , checkMaybeMsg
  , checkRequired
  , checkRequiredMsg
    -- * Classes
  , Show(..)
  , Eq(..)
  , Ord(..)
  , Enum(..)
  , Bounded(..)
  , Functor(..)
  , Monad(..)
  , Monoid(..)
  , Typeable(..)
  , Data(..)
  , LoopType
    -- * Modules
  , module P'
  , module Data.Serialize.VarInt
  ) where

import Control.Monad (ap, liftM)

import Data.Data                (Data(..),Typeable(..))
import Data.Word                (Word32, Word64)
import Data.Int                 (Int32,  Int64)
import Data.Ord
import Data.Monoid              (Monoid(..))
import Data.Sequence            (Seq,singleton)
import Data.ByteString          (ByteString)
import Data.Foldable            (mapM_)
import Data.Traversable         (mapM)

import Data.Protobuf.Classes    as P'
import Data.Serialize           as P'
import Data.Serialize.Protobuf  as P'
import Data.Serialize.VarInt
import Prelude hiding (mapM,mapM_)


-- | Type of worker function in the generated code. 
type LoopType v = v Unchecked -> Get (v Unchecked)


-- | Check optional value
checkMaybe :: Monad m => a -> Maybe a -> m (Maybe a)
checkMaybe x Nothing = return (Just x)
checkMaybe _ x       = return x
{-# INLINE checkMaybe #-}

-- | Check optional value
checkMaybeMsg :: (Message a, Monad m) =>  Maybe (a Unchecked) -> m (Maybe (a Checked))
checkMaybeMsg Nothing  = return Nothing
checkMaybeMsg (Just x) = liftM Just (checkReq x)
{-# INLINE checkMaybeMsg #-}

-- | Check required value
checkRequired :: Monad m => Required a -> m a
checkRequired NotSet      = fail "Field is not set!"
checkRequired (Present x) = return x
{-# INLINE checkRequired #-}

-- | Check required message
checkRequiredMsg :: (Message a, Monad m) => Required (a Unchecked) -> m (a Checked)
checkRequiredMsg NotSet      = fail "Field is not set!"
checkRequiredMsg (Present x) = checkReq x
{-# INLINE checkRequiredMsg #-}
