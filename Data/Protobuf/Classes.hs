module Data.Protobuf.Classes (
    PbEnum(..)
  , Message(..)
  , MessageEncode(..)
  ) where

import Data.Word
import Data.Int
import Data.Serialize.Get (Get)
import Data.Serialize.Put (Put)

-- | Type class for protobuf enums. It's possible to convert them to
--   'Int' and back
class PbEnum a where
  fromPbEnum :: a   -> Int
  toPbEnum   :: Int -> a

-- | API for messages.
class Message a where
  -- | Default value
  def :: a
  -- | Merge messages. Default implementation picks last value
  mergeMsg :: a -> a -> a
  mergeMsg _ = id
  {-# INLINE mergeMsg #-}



instance Message Bool   where def = False

instance Message Word   where def = 0
instance Message Word8  where def = 0
instance Message Word16 where def = 0
instance Message Word32 where def = 0
instance Message Word64 where def = 0

instance Message Int    where def = 0
instance Message Int8   where def = 0
instance Message Int16  where def = 0
instance Message Int32  where def = 0
instance Message Int64  where def = 0

instance Message a => Message (Maybe a) where
  def = Nothing
  mergeMsg Nothing  x        = x
  mergeMsg x        Nothing  = x
  mergeMsg (Just x) (Just y) = Just $ mergeMsg x y
  {-# INLINE mergeMsg #-}

instance Message [a] where 
  def      = []
  mergeMsg = (++)



-- | Serialization/deserialization of message
class MessageEncode a where
  getMessage :: Get a
  putMessage :: a -> Put
