{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
-- | Define utils for serializtion of protobuf message
module Data.Protobuf.Serialize.Protobuf (
    -- * Data types
    WireTag(..)
  , putWithWireTag
    -- * Getters
  , skipUnknownField
  , getPacked
  , putPacked
  , getPbString
  , putPbString
  , getPbBytestring
  , putPbBytestring
  -- , getDelimMessage
  , putOptional
    -- * Combinators for the mutable accumulator
  , getRecords
  , writeRequired
  , writeOptional
  , writeRepeated
  , writeRepeatedPacked
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Bits
import Data.ByteString          (ByteString)
import qualified Data.ByteString as BS
import Data.Serialize
import Data.Protobuf.Serialize.VarInt
import qualified Data.Sequence as Seq
import           Data.Sequence   (Seq,(|>),(><))
import qualified Data.Foldable as F
import qualified Data.Vector.HFixed as H
import GHC.TypeLits


import Data.Protobuf.API




----------------------------------------------------------------
-- Primitive parsers
----------------------------------------------------------------

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

-- | Get PB encoded string
getPbString :: Get String
getPbString = do
  n <- getVarInt
  isolate n getChars

putPbString :: String -> Put
putPbString str = do
  putVarInt (length str)
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

-- -- | Decode delimited message
-- getDelimMessage :: Message m => Get (m Unchecked)
-- getDelimMessage = do
--   n <- getVarInt
--   isolate n getMessage

putOptional :: (a -> Put) -> Maybe a -> Put
putOptional putter (Just x) = putter x
putOptional _      Nothing  = return ()
{-# INLINE putOptional #-}



----------------------------------------------------------------
-- Combinators
----------------------------------------------------------------


-- | Combinator for stateful parsing of message 
getRecords
  :: (WireTag -> a -> Get a) -- ^ Update function
  -> a                       -- ^ Initial state
  -> Get a
getRecords step new
  = loop new
  where
    loop st = do
      f <- isEmpty
      if f then return st
           else do wt <- get
                   loop =<< step wt st



-- | Write required field into accumulator
writeRequired :: (H.IdxVal n (FieldTypes msg) ~ a)
              => Sing n
              -> Get a
              -> MutableMsg msg
              -> Get (MutableMsg msg)
writeRequired n getter st = do
  a <- getter
  let go (MutableMsg marr ref) =
        MutableMsg (marr >>= (\arr -> H.writeMutableHVec arr n a >> return arr))
                   ref
  return $ go st

-- | Write optional field into accumulator
writeOptional :: (H.IdxVal n (FieldTypes msg) ~ Maybe a)
              => Sing n
              -> Get a
              -> MutableMsg msg
              -> Get (MutableMsg msg)
writeOptional n getter st = do
  a <- getter
  let go (MutableMsg marr ref) =
        MutableMsg (marr >>= (\arr -> H.writeMutableHVec arr n (Just a) >> return arr))
                   ref
  return $ go st

-- | Write repeated field into accumulator
writeRepeated :: (H.IdxVal n (FieldTypes msg) ~ Seq a)
              => Sing n
              -> Get a
              -> MutableMsg msg
              -> Get (MutableMsg msg)
writeRepeated n getter st = do
  a <- getter
  let go (MutableMsg marr ref) =
        MutableMsg (marr >>= (\arr -> H.modifyMutableHVec arr n (\sq -> sq |> a) >> return arr))
                   ref
  return $ go st


-- | Write repeated field into accumulator
writeRepeatedPacked :: (H.IdxVal n (FieldTypes msg) ~ Seq a)
                    => Sing n
                    -> Get a
                    -> MutableMsg msg
                    -> Get (MutableMsg msg)
writeRepeatedPacked n getter st = do
  a <- getPacked getter
  let go (MutableMsg marr ref) =
        MutableMsg (marr >>= (\arr -> H.modifyMutableHVec arr n (\sq -> sq >< a) >> return arr))
                   ref
  return $ go st
