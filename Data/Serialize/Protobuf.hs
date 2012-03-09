-- | Define utils for serializtion of protobuf message
module Data.Serialize.Protobuf ( 
    -- * Wiretag
    WireTag(..)
  , getWireTag
  , skipUnknownField
  ) where

import Control.Monad
import Data.Bits

import Data.Serialize.Get
import Data.Serialize.Put
import Data.Serialize.VarInt


getVarInt :: Get Int
getVarInt = undefined

data WireTag = WireTag {-# UNPACK #-} !Int {-# UNPACK #-} !Int

getWireTag :: Get WireTag
getWireTag = do
  i <- getVarInt
  return $! WireTag (0x07 .&. i) (i `shiftR` 3)

getSequence :: Get a -> Get [a]
getSequence getter = do
  n <- getVarInt
  replicateM n getter

skipUnknownField :: WireTag -> Get ()
skipUnknownField w = do
  return ()

