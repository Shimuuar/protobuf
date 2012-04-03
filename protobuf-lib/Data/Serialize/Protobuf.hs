-- | Define utils for serializtion of protobuf message
module Data.Serialize.Protobuf (
    -- * Data types
    WireTag(..)
  , putWithWireTag
    -- * Getters
  , skipUnknownField
  , getPacked
  , putPacked
  , getPbString
  , putPbString
  , getPbEnum
  , putPbEnum
  , getPbBytestring
  , putPbBytestring
  , getDelimMessage
  , putOptional
  ) where

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.ByteString          (ByteString)
import qualified Data.ByteString as BS
import Data.Serialize
import Data.Serialize.VarInt
import qualified Data.Sequence as Seq
import           Data.Sequence   (Seq,(|>))
import qualified Data.Foldable as F

import Data.Protobuf.Classes



-- | Wire tag. It's pair of message tags and type tag
data WireTag = WireTag {-# UNPACK #-} !Int {-# UNPACK #-} !Int
               deriving Show

instance Serialize WireTag where
  get = do
    i <- getVarInt
    return $! WireTag (i `shiftR` 3) (0x07 .&. i)
  put (WireTag t w) = putVarInt $ (t `shiftL` 3) .|. w

-- | Put wire atf 
putWithWireTag :: Int 
               -> Int 
               -> (a -> Put)
               -> a -> Put
{-# INLINE putWithWireTag #-}
putWithWireTag tag wt putter = \x -> do
  put (WireTag tag wt)
  putter x

-- | Get base-128 encoded int
getVarInt :: Get Int
getVarInt = fromIntegral <$> getVarWord64

putVarInt :: Int -> Put
putVarInt = putVarWord64 . fromIntegral 

-- | Skip unknow field
skipUnknownField :: WireTag -> Get ()
skipUnknownField (WireTag _ t) =
  case t of
    0 -> void getVarInt
    1 -> skip 8
    2 -> skip =<< getVarInt
    3 -> fail "Groups are not supported"
    4 -> fail "Groups are not supported"
    5 -> skip 4
    _ -> fail ("Bad wire tag: " ++ show t)

-- | Get packed sequence
getPacked :: Get a -> Get (Seq a)
getPacked getter = do
  n <- getVarInt
  isolate n $ getSeq Seq.empty getter

putPacked :: (a -> Put) -> Seq a -> Put
putPacked putter
  = putPbBytestring . runPut . F.mapM_ putter

-- Worker for getPacked
getSeq :: Seq a -> Get a -> Get (Seq a)
getSeq s getter = do
  f <- isEmpty
  if f
    then return s
    else do x <- getter
            getSeq (s |> x) getter

-- | Get protocol buffers enumeration
getPbEnum :: PbEnum a => Get a
getPbEnum = toPbEnum <$> getVarInt
{-# INLINE getPbEnum #-}

-- | Encode protocol buffers enumeration
putPbEnum :: PbEnum a => a -> Put
putPbEnum = putVarInt . fromPbEnum
{-# INLINE putPbEnum #-}

-- | Get PB encoded string
getPbString :: Get String
getPbString = do
  n <- getVarInt
  isolate n getChars

putPbString :: String -> Put
putPbString str = do
  put (length str)
  mapM_ put str

-- worker for getPbString
getChars :: Get String
getChars = do
  f <- isEmpty
  if f
    then return []
    else (:) <$> get <*> getChars

-- | Get PB encoded bytestring
getPbBytestring :: Get ByteString
getPbBytestring = getByteString =<< getVarInt

-- | Get PB encoded bytestring
putPbBytestring :: ByteString -> Put
putPbBytestring bs = putVarInt (BS.length bs) >> putByteString bs

-- | Decode delimited message
getDelimMessage :: Message m => Get (m Unchecked)
getDelimMessage = do
  n <- getVarInt
  isolate n getMessage

putOptional :: (a -> Put) -> Maybe a -> Put
putOptional putter (Just x) = putter x
putOptional _      Nothing  = return ()
{-# INLINE putOptional #-}
