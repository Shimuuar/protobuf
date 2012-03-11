-- | This module reexport everything which is needed for message
--   definition in generated code. There is no need import this module
--   in user code.
module Data.Protobuf.Imports (
    -- * Types
    Word32
  , Word64
  , Int32
  , Int64
  , Double
  , Float
  , String
  , Maybe(..)
  , Seq
  , ByteString
  , singleton
    -- Classes
  , Show(..)
  , Eq(..)
  , Ord(..)
  , Functor(..)
  , Monad(..)
  , Monoid(..)
  , comparing
  , Typeable(..)
  , Data(..)
  , module Data.Serialize.Get
  , module Data.Serialize.IEEE754
    -- 
  , module Data.Protobuf.Classes
  , module Data.Serialize.Protobuf
  , module Data.Serialize.VarInt
  , LoopType
  ) where

import Data.Word
import Data.Int
import Data.Data
import Data.Ord
import Data.Monoid
import Data.Sequence            (Seq,singleton)
import Data.ByteString          (ByteString)

import Data.Protobuf.Classes
import Data.Serialize.Get
import Data.Serialize.IEEE754
import Data.Serialize.Protobuf
import Data.Serialize.VarInt


type LoopType v = (v Required) -> Get (v Required)