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
module Data.Protobuf.API where

import Data.Vector.HFixed (HVector,Elems)
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
data family Message (msg :: Symbol) :: *

-- | Haskell types of all fields in message.
type family FieldTypes (msg :: Symbol) :: [*] 

-- | Protocol buffers message
class (HVector (Message msg), Elems (Message msg) ~ FieldTypes msg) => Protobuf (msg :: Symbol) where
  
  
-- | Access to fields of the message.     
class Field (msg :: Symbol) (fld :: Symbol) where
  type FieldTy msg fld :: *
