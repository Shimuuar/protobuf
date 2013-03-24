-- |
-- Variable width integer encoding which are used in google protobuf.
module Data.Protobuf.Serialize.VarInt ( 
    -- * Unsigned ints
    getVarWord32
  , getVarWord64
  , putVarWord32
  , putVarWord64
    -- * Signed ints
  , getVarInt32
  , getVarInt64
  , putVarInt32
  , putVarInt64
    -- * Zig-zag encoded ints
  , getZigzag32
  , getZigzag64
  , putZigzag32
  , putZigzag64
    -- * Booleans
  , getVarBool
  , putVarBool
  ) where

import Control.Applicative

import Data.Bits
import Data.Int
import Data.Word

import Data.Serialize.Get
import Data.Serialize.Put



getVarWord :: (Bits a, Num a) => Get a
{-# INLINE getVarWord #-}
getVarWord = do
  w <- fromIntegral <$> getWord8
  if not (testBit w 7)
    then return w
    else loop 7 (w .&. 0x7f)
  where
    loop n b = do
      w <- fromIntegral <$> getWord8
      if not (testBit w 7)
        then return $! (w `shiftL` n) .|. b
        else loop (n+7) (((w .&. 0x7f) `shiftL` n) .|. b)

putVarWord :: (Bits a, Integral a) => a -> Put
{-# INLINE putVarWord #-}
putVarWord n
  | n < 128     = putWord8 $ fromIntegral n
  | otherwise   = do putWord8 $ 0x80 .|. fromIntegral (0x7f .&. n)
                     putVarWord (n `shiftR` 7)


----------------------------------------------------------------
-- Concrete types
----------------------------------------------------------------

-- | Decode 32-bit 128-base unsigned integer.
getVarWord32 :: Get Word32
getVarWord32 = getVarWord

-- | Encode 32-bit 128-base unsigned integer.
putVarWord32 :: Word32 -> Put
putVarWord32 = putVarWord

-- | Decode 64-bit 128-base unsigned integer.
getVarWord64 :: Get Word64
getVarWord64 = getVarWord

-- | Encode 64-bit 128-base unsigned integer.
putVarWord64 :: Word64 -> Put
putVarWord64 = putVarWord



-- | Decode 32-bit 128-base signed integer.
getVarInt32 :: Get Int32
getVarInt32 = fromIntegral <$> getVarWord32

-- | Encode 32-bit 128-base signed integer.
putVarInt32 :: Int32 -> Put
putVarInt32 = putVarWord32 . fromIntegral

-- | Decode 32-bit 128-base signed integer.
getVarInt64 :: Get Int64
getVarInt64 = fromIntegral <$> getVarWord64

-- | Encode 32-bit 128-base signed integer.
putVarInt64 :: Int64 -> Put
putVarInt64 = putVarWord64 . fromIntegral



-- | Decode zigzag encoded 32-bit integer.
getZigzag32 :: Get Int32
getZigzag32 = dec32 <$> getVarWord32

-- | Encode zigzag encoded 32-bit integer.
putZigzag32 :: Int32 -> Put
putZigzag32 = putVarWord32 . enc32

-- | Decode zigzag encoded 32-bit integer.
getZigzag64 :: Get Int64
getZigzag64 = dec64 <$> getVarWord64

-- | Encode zigzag encoded 32-bit integer.
putZigzag64 :: Int64 -> Put
putZigzag64 = putVarWord64 . enc64


getVarBool :: Get Bool
getVarBool = do
  n <- getVarWord32
  return $! n /= 0

putVarBool :: Bool -> Put
putVarBool True  = putVarWord32 1
putVarBool False = putVarWord32 0

dec32 :: Word32 -> Int32
dec32 n 
  | odd n     = complement (fromIntegral $ n `shiftR` 1)
  | otherwise = fromIntegral (n `shiftR` 1)

dec64 :: Word64 -> Int64
dec64 n 
  | odd n     = complement (fromIntegral $ n `shiftR` 1)
  | otherwise = fromIntegral (n `shiftR` 1)


enc32 :: Int32 -> Word32
enc32 n =   fromIntegral (n `shiftL` 1) 
      `xor` fromIntegral (n `shiftR` 31)

enc64 :: Int64 -> Word64
enc64 n =   fromIntegral (n `shiftL` 1) 
      `xor` fromIntegral (n `shiftR` 63)
