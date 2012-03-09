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
    -- 
  , module Data.Protobuf.Classes
  , module Data.Serialize.Protobuf
  , module Data.Serialize.VarInt
  ) where

import Data.Word
import Data.Int
import Data.Data
import Data.Ord
import Data.Monoid

import Data.Protobuf.Classes
import Data.Serialize.Protobuf
import Data.Serialize.VarInt
