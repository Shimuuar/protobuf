{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
-- |
-- API for working with protobuf messages. They are encoded at the
-- type level
--
-- Following example of message is taken from google's protobuf
-- documentation and will referred to below.
--
-- > message Person {
-- >   required string name  = 1;
-- >   required int32  id    = 2;
-- >   optional string email = 3;
-- >
-- >   enum PhoneType {
-- >     MOBILE = 0;
-- >     HOME = 1;
-- >     WORK = 2;
-- >   }
-- >
-- >   message PhoneNumber {
-- >     required string    number = 1;
-- >     optional PhoneType type   = 2 [default = HOME];
-- >   }
-- >
-- >   repeated PhoneNumber phone = 4;
-- > }
module Data.Protobuf.API (
    Message
  , FieldTypes
  , Field(..)
  , PbEnum(..)
    -- * Serialization
  , Protobuf(..)
  , getMessage
  , decodeMessage
  , decodeMessage_
    -- * Helpers
  , MutableMsg(..)
  , freezeMutableMsg
  ) where

import Control.Monad.ST        (ST,runST)
import Data.STRef
import Data.Serialize          (Get,Put,runGet)
import Data.ByteString         (ByteString)
import Data.Vector.HFixed      (HVector,Elems)
import Data.Vector.HFixed.HVec (MutableHVec)
import qualified Data.Vector.HFixed      as H
import qualified Data.Vector.HFixed.HVec as H
import GHC.TypeLits


----------------------------------------------------------------
-- Type level description of messages
----------------------------------------------------------------

-- | Data family which maps fully qualified names to data types. For
--   example messages @Person@ and "@Person.PhoneNumber@ will have
--   types:
--
-- > Message "Person
-- > Message "Person.PhoneNumber"
--
--   By default messages are newtype wrappers over heterogenous
--   vectors with corresponding types.
--
--   Enumerations however are more difficult. Most natural way is to
--   generate data type with constructors with names corresponding to
--   enum fields. But then we can have constructors' name clashes.
--
--   It seems there is no single option that could reasonably work in
--   every case. So function that generate instances have selection of
--   options to handle name clashes.
data family Message (msg :: Symbol) :: *

-- | Haskell types of all fields in message.
type family FieldTypes (msg :: Symbol) :: [*]

-- | Type class for protocol buffers enumerations
class PbEnum a where
  fromPbEnum :: a -> Int
  toPbEnum   :: Int -> Maybe a

-- | Access to fields of the message.
class Field (msg :: Symbol) (fld :: Symbol) where
  type FieldTy msg fld :: *
  field :: (Functor f, FieldTy msg fld ~ a)
        => Sing fld
        -> (a -> f a)
        -> (Message msg -> f (Message msg))


----------------------------------------------------------------
-- Parsing
----------------------------------------------------------------

-- | Data type is protocol buffer object.  This type class provide
--   serialization and deserialization. Access to fields is provided
--   by 'Field' type class.
class ( HVector (Message msg)
      , Elems (Message msg) ~ FieldTypes msg
      , HVector (H.HVec (FieldTypes msg))
      ) => Protobuf (msg :: Symbol) where
  -- | Parser for the message
  getMessageST :: Get (MutableMsg msg)
  -- | Encoder for the message
  serialize :: Message msg -> Put

-- | Decode protobuf message.
getMessage :: Protobuf msg => Get (Message msg)
getMessage = do
  freezeMutableMsg =<< getMessageST

-- | Decode protobuf message from bytestring
decodeMessage :: Protobuf msg => ByteString -> Either String (Message msg)
decodeMessage bs = runGet getMessage bs

-- | Decode protobuf message from bytestring. Throws error if decoding
--  failed.
decodeMessage_ :: Protobuf msg => ByteString -> Message msg
decodeMessage_
  = either error id . decodeMessage


-- | Mutable version of message it's used internally during message
--   decoding.
data MutableMsg (msg :: Symbol) =
  MutableMsg
  (forall s. ST s (MutableHVec s (FieldTypes msg)))
  ()
  -- (ST s (STRef s Int))

freezeMutableMsg :: Protobuf msg => MutableMsg msg -> Get (Message msg)
freezeMutableMsg (MutableMsg marr _)
  = return $ H.convert $ runST $ H.unsafeFreezeHVec =<< marr
