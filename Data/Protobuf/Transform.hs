-- | Transofrmation of protobug AST
module Data.Protobuf.Transform where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

import qualified Data.Foldable as F
import qualified Data.Map      as Map
import           Data.Map        (Map)
import Data.Data                 (Data)
import Data.Monoid
import Data.Generics.Uniplate.Data

import Data.Protobuf.AST
import Data.Protobuf.AST
import Data.Protobuf.Grammar.Parser
import Data.Protobuf.Grammar.Lexer

import System.Directory


----------------------------------------------------------------
-- Validation
----------------------------------------------------------------

validate :: FileMap -> PbMonad ()
validate pbMap = do 
  forM_ (Map.toList pbMap) $ \(nm, pb) -> do
    -- Only one package is allowed
    case [p | Package p <- pb] of
      []  -> return ()
      [_] -> return ()
      _   -> throwError $ nm ++ ": duplicate package entry"



----------------------------------------------------------------
-- Transformations
----------------------------------------------------------------


-- | Remove imports from tree. 
stripImports :: [Protobuf] -> [Protobuf]
stripImports = filter notImport
  where 
    notImport (Import _) = False
    notImport


-- resolveNames 