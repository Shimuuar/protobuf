-- | Data types for transformations
module Data.Protobuf.Types ( 
    PbMonad
  , PbContext(..)
  , FileMap
  ) where

import Control.Monad.Trans.Reader
import Data.Map   (Map)

import Data.Protobuf.AST



-- | Monad for reading and transformations
type PbMonad = ReaderT PbContext IO

-- | Context for protobuf
data PbContext = PbContext { includePaths :: [String]
                           }

type FileMap = Map FilePath [Protobuf]
