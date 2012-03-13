-- |
--  API for generated code.
--
-- * Required fields are most tricky. Since 
--
-- * Optionl fields are represented as 'Maybe'
--
-- * Repeated fields are represented as 'Seq' from Data.Sequence.
module Data.Protobuf.Classes (
    -- * Data types
    Required(..)
  , Val(..)
    -- * Classes
  , PbEnum(..)
  , Default(..)
  , Message(..)
  , MessageField(..)
  ) where

import qualified Data.ByteString as B
import Data.Word
import Data.Int
import Data.Monoid        (Monoid(..))
import Data.Sequence      (Seq)
import Data.Serialize.Get (Get)
import Data.Serialize.Put (Put)

-- | Maybe-like data type for tracking whether required field is set
--   or not.
data Required a 
  = Present a
  | NotSet
    deriving (Show)

instance Functor Required where
  fmap f (Present x) = Present (f x)
  fmap _ NotSet      = NotSet

-- | Wrapper for requieed type after checking.
newtype Val a = Val a
                deriving (Show)

----------------------------------------------------------------
-- Type classes
----------------------------------------------------------------

-- | Type class for protobuf enums. It's possible to convert them to
--   'Int' and back
class PbEnum a where
  fromPbEnum :: a   -> Int
  toPbEnum   :: Int -> a


-- | Default value for field
class Default a where
  -- | Default value
  def :: a


-- | Serialization/deserialization of message
class Message a where
  -- | Deserialize message
  getMessage :: Get (a Required)
  -- | Check that all required fields are present
  checkReq   :: Monad m => a Required -> m (a Val)
  -- | Serialize message
  putMessage :: (a Val) -> Put



----------------------------------------------------------------
-- Implementation type classes

-- | This is implementation specific type class for merging field.
class MessageField f where
  mergeField :: f -> f -> f
  mergeField _ x = x
  {-# INLINE mergeField #-}



----------------------------------------------------------------
-- Instances for MessageField

instance MessageField Bool
instance MessageField Word32
instance MessageField Word64
instance MessageField Int32
instance MessageField Int64
instance MessageField [a] where
  mergeField = (++)
instance MessageField B.ByteString where
  mergeField = mappend

instance MessageField a => MessageField (Maybe a) where
  mergeField (Just a) (Just b) = Just $ mergeField a b
  mergeField Nothing  x        = x
  mergeField x        Nothing  = x

instance MessageField a => MessageField (Required a) where
  mergeField (Present a) (Present b) = Present $ mergeField a b
  mergeField NotSet  x       = x
  mergeField x       NotSet  = x

instance MessageField a => MessageField (Val a) where
  mergeField (Val a) (Val b) = Val $ mergeField a b

instance MessageField (Seq a) where
  mergeField = mappend

----------------------------------------------------------------
-- Instances for Default

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

instance Default (Required a) where
  def = NotSet
instance Default (Maybe a) where
  def = Nothing
instance Default [a] where
  def = []
instance Default (Seq a) where
  def = mempty
