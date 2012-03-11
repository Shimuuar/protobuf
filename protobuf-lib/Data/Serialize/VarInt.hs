-- | Variable width integer encoding which are used in google protobuf
module Data.Serialize.VarInt where

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

getVarWord32 :: Get Word32
getVarWord32 = getVarWord

putVarWord32 :: Word32 -> Put
putVarWord32 = putVarWord

getVarWord64 :: Get Word64
getVarWord64 = getVarWord

putVarWord64 :: Word64 -> Put
putVarWord64 = putVarWord


getVarInt32 :: Get Int32
getVarInt32 = fromIntegral <$> getVarWord32

putVarInt32 :: Int32 -> Put
putVarInt32 = putVarWord32 . fromIntegral

getVarInt64 :: Get Int64
getVarInt64 = fromIntegral <$> getVarWord64

putVarInt64 :: Int64 -> Put
putVarInt64 = putVarWord64 . fromIntegral


getZigzag32 :: Get Int32
getZigzag32 = fromIntegral <$> getVarWord32

putZigzag32 :: Int32 -> Put
putZigzag32 = putVarWord32 . fromIntegral

getZigzag64 :: Get Int64
getZigzag64 = fromIntegral <$> getVarWord64

putZigzag64 :: Int64 -> Put
putZigzag64 = putVarWord64 . fromIntegral
