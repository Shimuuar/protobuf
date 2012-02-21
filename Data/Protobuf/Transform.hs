{-# LANGUAGE RecordWildCards #-}
-- | Transofrmation of protobug AST
module Data.Protobuf.Transform where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Error

import qualified Data.Foldable    as F
import qualified Data.Traversable as T
import qualified Data.Map      as Map
import           Data.Map        (Map)
import qualified Data.Set      as Set
import           Data.Set        (Set)
import Data.Data                 (Data)
import Data.List
import Data.Function
import Data.Monoid
import Data.Ord
import Data.Generics.Uniplate.Data

import Data.Protobuf.AST
import Data.Protobuf.Grammar.Parser
import Data.Protobuf.Grammar.Lexer
import Data.Protobuf.Types
import Data.Protobuf.FileIO



-- Name resolution
--
-- There are 3 kinds of names which all inhabit same namespace.
--
-- * Message names
--
-- * Enumeration names
--
-- * Enumeration fields
--
-- Message and enums are really same. They are name of types.
--
-- Goals of name resolution are to check that all names are already
-- known and to make all names fully qualified.


----------------------------------------------------------------
-- Build namespaces
----------------------------------------------------------------

-- Build namespaces.
--
--  * Collects all names defined in module
--  * Check that there are no duplicates
buildNamespace :: Bundle () -> PbMonad (Bundle Namespace)
buildNamespace b@(Bundle{..}) = do
  pkg <- T.mapM (\(PbFile pb _) -> PbFile pb <$> buildNamespaceWorker pb) packageMap
  return b { packageMap = pkg }


-- Build namespace for loaded module
buildNamespaceWorker :: [Protobuf] -> PbMonad Namespace
buildNamespaceWorker pb = do
  names <- collectErrors $ workerPackageNames pb
  case [ p | Package p <- pb ] of
    []      -> return $ names
    [ qid ] -> return $ foldr packageNamespace names qid
    _       -> throwError "Multiple package declaration"


-- Collect top level names
workerPackageNames :: [Protobuf] -> PbMonadE Namespace
workerPackageNames = foldM collect emptyNamespace
  where
    collect s (MessageDecl m) = workerMessageNames s m
    collect s (TopEnum     e) = workerEnumNames    s e
    collect s _               = return s


-- Collect Enum names
workerEnumNames :: Namespace -> EnumDecl -> PbMonadE Namespace
workerEnumNames set (EnumDecl nm flds) =
  flip insertName (EnumName nm)
    =<< foldM insertName set [EnumElem n | EnumField n _ <- flds]


-- Collect names in message
workerMessageNames :: Namespace -> Message -> PbMonadE Namespace
workerMessageNames set (Message nm flds) = do
  let insertMsg s (MessageField (Field _ _ n _ _)) = insertName s (FieldName n)
      insertMsg s (MessageEnum  e) = workerEnumNames    s e
      insertMsg s (Nested       m) = workerMessageNames s m
      insertMsg s _                = return s
  insertName set . MsgName nm =<< foldM insertMsg emptyNamespace flds





----------------------------------------------------------------
-- Resolve names
----------------------------------------------------------------
