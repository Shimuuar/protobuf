-- | This module reexport everything which is needed for message
--   definition in generated code. There is no need import this module
--   in user code.
module Data.Protobuf.Imports (
    -- * Types
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
  , singleton
  , undefined
    -- Classes
  , Show(..)
  , Eq(..)
  , Ord(..)
  , Functor(..)
  , Monad(..)
  , Monoid(..)
  , comparing
  , Typeable(..)
  , Data(..)
  , module Data.Serialize.Get
  , module Data.Serialize.IEEE754
    -- 
  , module Data.Protobuf.Classes
  , module Data.Serialize.Protobuf
  , module Data.Serialize.VarInt
  , LoopType
    --
  , checkMaybe
  , checkMaybeMsg
  , checkRequired
  , checkRequiredMsg
  , ap
  , mapM
  ) where

import Control.Monad (ap, liftM)

import Data.Word
import Data.Int
import Data.Data
import Data.Ord
import Data.Monoid
import Data.Sequence            (Seq,singleton)
import Data.ByteString          (ByteString)
import Data.Traversable         (mapM)

import Data.Protobuf.Classes
import Data.Serialize.Get
import Data.Serialize.IEEE754
import Data.Serialize.Protobuf
import Data.Serialize.VarInt
import Prelude hiding (mapM)

type LoopType v = (v Required) -> Get (v Required)

checkMaybe :: Monad m => a -> Maybe a -> m (Maybe a)
checkMaybe x Nothing = return (Just x)
checkMaybe _ x       = return x

checkMaybeMsg :: (Message a, Monad m) =>  Maybe (a Required) -> m (Maybe (a Val))
checkMaybeMsg Nothing  = return Nothing
checkMaybeMsg (Just x) = liftM Just (checkReq x)

checkRequired :: Monad m => Required a -> m (Val a)
checkRequired NotSet      = fail "Field is not set!"
checkRequired (Present x) = return $ Val x

checkRequiredMsg :: (Message a, Monad m) => Required (a Required) -> m (Val (a Val))
checkRequiredMsg NotSet      = fail "Field is not set!"
checkRequiredMsg (Present x) = liftM Val (checkReq x)