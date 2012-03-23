-- | This module reexport everything which is needed for message
--   definition in generated code. There is no need import this module
--   in user code.
module Data.Protobuf.Imports (
    -- * Data types
    Word32
  , Word64
  , Int32
  , Int64
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
    -- ** Decode functions
    -- *** Floating point values
  , getFloat32le
  , getFloat64le
  , putFloat32le
  , putFloat64le
    -- *** Varint encoded integrals
  , getVarWord32
  , getVarWord64
  , putVarWord32
  , putVarWord64
  , getVarInt32
  , getVarInt64
  , putVarInt32
  , putVarInt64
    -- *** Zig-zag encoded integrals
  , getZigzag32
  , getZigzag64
  , putZigzag32
  , putZigzag64
    -- ** Converwsion
  , checkMaybe
  , checkMaybeMsg
  , checkRequired
  , checkRequiredMsg
    -- * Classes
  , Show(..)
  , Eq(..)
  , Ord(..)
  , Functor(..)
  , Monad(..)
  , Monoid(..)
  , Typeable(..)
  , Data(..)
  , LoopType
    -- * Modules
  , module P'
  ) where

import Control.Monad (ap, liftM)

import Data.Data                (Data(..),Typeable(..))
import Data.Word                (Word32, Word64)
import Data.Int                 (Int32,  Int64)
import Data.Ord
import Data.Monoid              (Monoid(..))
import Data.Sequence            (Seq,singleton)
import Data.ByteString          (ByteString)
import Data.Traversable         (mapM)

import Data.Protobuf.Classes    as P'
import Data.Serialize           as P'
import Data.Serialize.Protobuf  as P'
import Data.Serialize.VarInt
import Prelude hiding (mapM)


-- | Type of worker function in the generated code. 
type LoopType v = v Required -> Get (v Required)

-- | Check optional value
checkMaybe :: Monad m => a -> Maybe a -> m (Maybe a)
checkMaybe x Nothing = return (Just x)
checkMaybe _ x       = return x
{-# INLINE checkMaybe #-}

-- | Check optional value
checkMaybeMsg :: (Message a, Monad m) =>  Maybe (a Required) -> m (Maybe (a Val))
checkMaybeMsg Nothing  = return Nothing
checkMaybeMsg (Just x) = liftM Just (checkReq x)
{-# INLINE checkMaybeMsg #-}

-- | Check required value
checkRequired :: Monad m => Required a -> m (Val a)
checkRequired NotSet      = fail "Field is not set!"
checkRequired (Present x) = return $ Val x
{-# INLINE checkRequired #-}

-- | Check required message
checkRequiredMsg :: (Message a, Monad m) => Required (a Required) -> m (Val (a Val))
checkRequiredMsg NotSet      = fail "Field is not set!"
checkRequiredMsg (Present x) = liftM Val (checkReq x)
{-# INLINE checkRequiredMsg #-}