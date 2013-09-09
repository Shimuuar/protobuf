{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
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
  , encodeMessage
  , decodeMessage
  , decodeMessage_
    -- * Helpers
  , MutableMsg(..)
  , freezeMutableMsg
  ) where

import Control.Monad.ST        (ST,runST)
import Data.STRef
import Data.Serialize          (Get,Put,runGet,runPut)
import Data.ByteString         (ByteString)
import Data.Vector.HFixed      (HVector,Elems)
import Data.Vector.HFixed.HVec (MutableHVec)
import Data.Typeable
import qualified Data.Vector.HFixed      as H
import qualified Data.Vector.HFixed.HVec as H

import GHC.TypeLits



----------------------------------------------------------------
-- Type level description of messages
----------------------------------------------------------------

-- | Type family which maps fully qualified names of protobuf messages
--   to haskell data types. For example messages /Person/ and
--   /Person.PhoneNumber/ will have types:
--
-- > Message "Person
-- > Message "Person.PhoneNumber"
--
--   Type family is needed to allow substituting predefined haskell
--   data types in place of automatically generated ones. If data type
--   is generated automatically it's a newtype wrapper over
--   heterogenous vector.
--
--   Enumerations however are more difficult. Most natural way is to
--   generate data type with constructors with names corresponding to
--   enum fields. But then we can have constructors' name clashes.
--
--   It seems there is no single option that could reasonably work in
--   every case. So function that generate instances have selection of
--   options to handle name clashes.
type family Message (msg :: Symbol) :: *

-- | Haskell types of all fields in a message.
type family FieldTypes (msg :: Symbol) :: [*]

-- | Type class for protocol buffers enumerations
class PbEnum a where
  fromPbEnum :: a -> Int
  toPbEnum   :: Int -> Maybe a

-- | Accessors to fields of the message. @msg@ is name of message and
--   @fld@ is name of field
class Field (msg :: Symbol) (fld :: Symbol) where
  -- | Haskell type of field.
  type FieldTy msg fld :: *
  -- | Twan van Laarhoven lens for message.
  field :: (Functor f, FieldTy msg fld ~ a)
        => Sing fld
        -> (a -> f a)
        -> (Message msg -> f (Message msg))



----------------------------------------------------------------
-- Parsing
----------------------------------------------------------------

-- | Data type is protocol buffer object. This type class provide
--   serialization and deserialization. Access to fields is provided
--   by 'Field' type class.
--
--   Message also must be a heterogeneous vector with correct
--   elements' types.
class ( HVector msg
      , Elems msg ~ FieldTypes (MessageName msg)
      , Message (MessageName msg) ~ msg
      ) => Protobuf (msg :: *) where
  -- | Protobuf name of message
  type MessageName msg :: Symbol
  -- | Parser for the message
  getMessageST :: Get (MutableMsg msg)
  -- | Encoder for the message
  serialize :: msg -> Put

-- | Decode protobuf message.
getMessage :: Protobuf msg => Get msg
getMessage = do
  freezeMutableMsg =<< getMessageST

-- | Encode protobuf message
encodeMessage :: Protobuf msg => msg -> ByteString
encodeMessage = runPut . serialize

-- | Decode protobuf message from bytestring
decodeMessage :: Protobuf msg => ByteString -> Either String msg
decodeMessage bs = runGet getMessage bs

-- | Decode protobuf message from bytestring. Throws error if decoding
--  failed.
decodeMessage_ :: Protobuf msg => ByteString -> msg
decodeMessage_
  = either error id . decodeMessage


-- | Mutable version of message it's used internally during message
--   decoding.
data MutableMsg (msg :: *) =
  MutableMsg
  (forall s. ST s (MutableHVec s (FieldTypes (MessageName msg))))
  ()
  -- (ST s (STRef s Int))


freezeMutableMsg :: (Protobuf msg)
                 => MutableMsg msg -> Get msg
freezeMutableMsg (MutableMsg marr _)
  = return $ H.convert $ runST $ H.unsafeFreezeHVec =<< marr
