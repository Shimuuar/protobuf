module Data.Protobuf.Serialize.Message (
    PbMessage(..)
  , PbMessageBody(..)
  , getPbMessages
  ) where

import Control.Applicative
import Data.Serialize
import Data.Protobuf.Serialize.Protobuf
import Data.Protobuf.Serialize.VarInt

import Data.ByteString (ByteString)
import Data.Int


data PbMessage = PbMessage WireTag PbMessageBody
                 deriving Show

-- | Representation of on-wire protobuf messages
data PbMessageBody
  = PbVarint   Int
  | PbFixed64  Int64
  | PbLenDelim ByteString
  | PbFixed32  Int32
  | PbNested   [PbMessage]
  deriving Show

getPbMessages :: Get [PbMessage]
getPbMessages = do
  f <- isEmpty
  if f
    then return []
    else (:) <$> getMsg <*> getPbMessages


getMsg :: Get PbMessage
getMsg = do
  wt <- get
  b  <- case wt of
          WireTag _ 0 -> PbVarint . fromIntegral <$> getVarInt64
          WireTag _ 1 -> PbFixed64 <$> get
          WireTag _ 2 -> do n <- fromIntegral <$> getVarInt64
                            PbLenDelim <$> getByteString n
          WireTag _ 5 -> PbFixed32 <$> get
          _           -> error "Bad format"
  return $ PbMessage wt b