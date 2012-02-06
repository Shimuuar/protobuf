-- | Data types for transformations
module Data.Protobuf.Types ( 
    PbMonad
  , askContext
  , PbContext(..)
  , FileMap
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Error
import Data.Map   (Map)

import Data.Protobuf.AST



-- | Monad for reading and transformations
type PbMonad = 
  ErrorT String
   (ReaderT PbContext IO)

-- | Ask for context
askContext :: PbMonad PbContext
askContext = lift ask

-- | Context for protobuf
data PbContext = PbContext { includePaths :: [String]
                           }

-- | Map of file path to AST
type FileMap = Map FilePath [Protobuf]
