{-# LANGUAGE FlexibleContexts #-}
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
  , putOptional
  , getDelimited
  , getPbEnum
  , putPbEnum
    -- * Combinators for the mutable accumulator
  , getRecords
  , writeRequired
  , writeOptional
  , writeRepeated
  , writeRepeatedPacked
  ) where

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.ByteString          (ByteString)
import qualified Data.ByteString as BS
import Data.Serialize
import Data.Protobuf.Serialize.VarInt
import Data.Text          (Text)
import Data.Text.Encoding (decodeUtf8,encodeUtf8)
import qualified Data.Sequence as Seq
import           Data.Sequence   (Seq,(|>),(><))
import qualified Data.Foldable as F
import qualified Data.Vector.Fixed        as F (Arity)
import qualified Data.Vector.HFixed       as H
import qualified Data.Vector.HFixed.HVec  as H
import qualified Data.Vector.HFixed.Class as H (NatIso(..))
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
getPbString :: Get Text
getPbString = do
  n  <- getVarInt
  bs <- getByteString n
  return $! decodeUtf8 bs

putPbString :: Text -> Put
putPbString str = do
  let bs = encodeUtf8 str
  putVarInt (BS.length bs)
  putByteString bs

-- | Get PB encoded bytestring
getPbBytestring :: Get ByteString
getPbBytestring = getByteString =<< getVarInt

-- | Get PB encoded bytestring
putPbBytestring :: ByteString -> Put
putPbBytestring bs = putVarInt (BS.length bs) >> putByteString bs

putOptional :: (a -> Put) -> Maybe a -> Put
putOptional putter (Just x) = putter x
putOptional _      Nothing  = return ()
{-# INLINE putOptional #-}

getDelimited :: Protobuf m => Get m
{-# INLINE getDelimited #-}
getDelimited = do
  n <- getVarInt
  isolate n getMessage

getPbEnum :: PbEnum m => Get m
{-# INLINE getPbEnum #-}
getPbEnum = do
  n <- getVarInt
  case toPbEnum n of
    Just m  -> return m
    Nothing -> fail "Bad enum"

putPbEnum :: PbEnum m => m -> Put
{-# INLINE putPbEnum #-}
putPbEnum = putVarInt . fromPbEnum



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
writeRequired :: forall n a msg. ( H.ValueAt (H.ToPeano n) (FieldTypes (MessageName msg)) ~ a
                                 , H.NatIso  (H.ToPeano n) n
                                 , SingI n
                                 , F.Arity (H.ToPeano n))
              => Sing n
              -> Get a
              -> MutableMsg msg
              -> Get (MutableMsg msg)
writeRequired _ getter st = do
  a <- getter
  let go (MutableMsg marr ref) =
        MutableMsg (marr >>= (\arr -> H.writeMutableHVec arr (H.natIdx (sing :: Sing n)) a >> return arr))
                   ref
  return $ go st

-- | Write optional field into accumulator
writeOptional :: forall n a msg. ( H.ValueAt (H.ToPeano n) (FieldTypes (MessageName msg)) ~ Maybe a
                                 , H.NatIso  (H.ToPeano n) n
                                 , SingI n
                                 , F.Arity (H.ToPeano n))
              => Sing n
              -> Get a
              -> MutableMsg msg
              -> Get (MutableMsg msg)
writeOptional _ getter st = do
  a <- getter
  let go (MutableMsg marr ref) =
        MutableMsg (marr >>= (\arr -> H.writeMutableHVec arr (H.natIdx (sing :: Sing n)) (Just a) >> return arr))
                   ref
  return $ go st

-- | Write repeated field into accumulator
writeRepeated :: forall n a msg. ( H.ValueAt (H.ToPeano n) (FieldTypes (MessageName msg)) ~ Seq a
                                 , H.NatIso  (H.ToPeano n) n
                                 , SingI n
                                 , F.Arity (H.ToPeano n))
              => Sing n
              -> Get a
              -> MutableMsg msg
              -> Get (MutableMsg msg)
writeRepeated _ getter st = do
  a <- getter
  let go (MutableMsg marr ref) =
        MutableMsg (marr >>= (\arr -> H.modifyMutableHVec' arr (H.natIdx (sing :: Sing n)) (\sq -> sq |> a) >> return arr))
                   ref
  return $ go st


-- | Write repeated field into accumulator
writeRepeatedPacked :: forall n a msg. ( H.ValueAt (H.ToPeano n) (FieldTypes (MessageName msg)) ~ Seq a
                                       , H.NatIso  (H.ToPeano n) n
                                       , SingI n
                                       , F.Arity (H.ToPeano n))
                    => Sing n
                    -> Get a
                    -> MutableMsg msg
                    -> Get (MutableMsg msg)
writeRepeatedPacked _ getter st = do
  a <- getPacked getter
  let go (MutableMsg marr ref) =
        MutableMsg (marr >>= (\arr -> H.modifyMutableHVec' arr (H.natIdx (sing :: Sing n)) (\sq -> sq >< a) >> return arr))
                   ref
  return $ go st
