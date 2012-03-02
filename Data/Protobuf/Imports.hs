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
    -- Classes
  , Show(..)
  , Eq(..)
  , Ord(..)
  , comparing
  , Typeable(..)
  , Data(..)
    -- 
  , module Data.Protobuf.Classes
  ) where

import Data.Word
import Data.Int
import Data.Data
import Data.Ord

import Data.Protobuf.Classes