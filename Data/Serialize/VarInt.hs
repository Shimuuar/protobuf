-- | Variable width integer encoding which are used in google protobuf
module Data.Serialize.VarInt where

import Control.Applicative

import Data.Bits
import Data.Serialize.Get
import Data.Serialize.Put
import Data.Word



getVarWord :: (Bits a, Num a) => Get a
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
putVarWord n
  | testBit n 7 = putWord8 $ fromIntegral n
  | otherwise   = do putWord8 $ 0xf0 .|. fromIntegral (0x7f .&. n)
                     putVarWord (n `shiftL` 7)
