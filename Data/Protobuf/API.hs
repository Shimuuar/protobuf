{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
  , Msg(..)
  , FieldTypes
  , Protobuf(..)
  , Field(..)
  ) where

import Data.Serialize     (Get,Put)
import Data.Vector.HFixed (HVector,Elems,Fun)
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
--   It maps both messages and enums.
type family Message (msg :: Symbol) :: *

newtype Msg (msg :: Symbol) = Msg (Message msg)

-- | Haskell types of all fields in message.
type family FieldTypes (msg :: Symbol) :: [*]

-- | Data type is protocol buffer object.  This type class provide
--   serialization and deserialization. Access to fields is provided
--   by 'Field' type class.
class (HVector (Message msg), Elems (Message msg) ~ FieldTypes msg
      ) => Protobuf (msg :: Symbol) where
  -- | Parser for the message
  deserialize :: Get (Msg msg)
  -- | Encoder for the message
  serialize :: Msg msg -> Put

-- | Access to fields of the message.
class Field (msg :: Symbol) (fld :: Symbol) where
  type FieldTy msg fld :: *
  getterF :: Fun (FieldTypes msg) (FieldTy msg fld)
