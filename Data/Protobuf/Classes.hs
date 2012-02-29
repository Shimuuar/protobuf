module Data.Protobuf.Classes (
  ) where

import Data.Word
import Data.Int
import Data.Serialize.Get (Get)
import Data.Serialize.Put (Put)


class Default a where
  def :: a
  
instance Default Bool   where def = False

instance Default Word   where def = 0
instance Default Word8  where def = 0
instance Default Word16 where def = 0
instance Default Word32 where def = 0
instance Default Word64 where def = 0

instance Default Int    where def = 0
instance Default Int8   where def = 0
instance Default Int16  where def = 0
instance Default Int32  where def = 0
instance Default Int64  where def = 0

instance Default [a]    where def = []



-- | Type class for protobuf enums
class PbEnum a where
  fromPbEnum :: a   -> Int
  toPbEnum   :: Int -> a

-- | Serialization/deserialization of message
class Message a where
  getMessage :: Get a
  putMessage :: a -> Put
