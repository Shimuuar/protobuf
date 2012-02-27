{-# LANGUAGE RecordWildCards #-}
-- | Transofrmation of protobug AST
module Data.Protobuf.Transform where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error

import qualified Data.Foldable    as F
import qualified Data.Traversable as T
import qualified Data.Map      as Map
import           Data.Map        (Map,(!))
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
-- import Data.Protobuf.FileIO
import Data.Protobuf.DataTree



----------------------------------------------------------------
-- Normalization
----------------------------------------------------------------

----------------------------------------------------------------
-- * Stage 1. Remove package declarations and move package name into
--   ProtobufFile declaration.
removePackage :: ProtobufFile a -> PbMonad (ProtobufFile a)
removePackage (ProtobufFile pb _ x) = do
  p <- case [ p | Package p <- pb ] of
         []   -> return []
         [qs] -> return qs
         _    -> throwError "Multiple package declarations"
  return $ ProtobufFile pb p x


----------------------------------------------------------------
-- * Stage 2. Build and cache namespaces. During this stage name
--   collisions are discovered and repored as errors. Package
--   namespace is added to global namespace.
buildNamespace :: ProtobufFile a -> PbMonad (ProtobufFile Namespace)
buildNamespace (ProtobufFile pb qs _) =
  collectErrors $ do
    (pb',ns) <- runNamespace $ mapM (collectPackageNames qs) pb
    return $ ProtobufFile pb' qs (foldr packageNamespace ns qs)

-- Collect all names in package
collectPackageNames :: [Identifier] -> Protobuf -> NameCollector Protobuf
collectPackageNames path (TopMessage m) =
  TopMessage <$> collectMessageNames path m
collectPackageNames path (TopEnum    e) =
  TopEnum    <$> collectEnumNames    path e
collectPackageNames _ x = return x

-- Get namspace for a message
collectMessageNames :: [Identifier] -> Message -> NameCollector Message
collectMessageNames path (Message name fields _) = do
  let path' = path ++ [name]
  (fs,ns) <- lift $ runNamespace $ mapM (collectFieldNames path') fields
  addName $ MsgName name ns
  return  $ Message name fs path'

-- Get namespace for an enum
collectEnumNames :: [Identifier] -> EnumDecl -> NameCollector EnumDecl
collectEnumNames path (EnumDecl name fields _) = do
  addName (EnumName name)
  mapM_ addName [ FieldName n | EnumField n _ <- fields] 
  return $ EnumDecl name fields path

-- Collect names from the fields
collectFieldNames :: [Identifier] -> MessageField -> NameCollector MessageField
collectFieldNames path f@(MessageField (Field _ _ n _ _)) =
  f <$ addName (FieldName n)
collectFieldNames path (Nested m) = do
  Nested <$> collectMessageNames path m
collectFieldNames path (MessageEnum e) =
  MessageEnum <$> collectEnumNames path e
collectFieldNames _ x = return x


type NameCollector = StateT Namespace PbMonadE

-- Get names
runNamespace :: StateT Namespace m a -> m (a, Namespace)
runNamespace = flip runStateT emptyNamespace

-- Add name into namespace
addName :: SomeName -> NameCollector ()
addName n =
  put =<< lift . flip insertName n =<< get


  
----------------------------------------------------------------
-- * Stage 3. Resolve imports and build global namespace. Name clashes
--   in import are discovered during this stage. After this stage each
--   protobuf file is self containted so we can discard bundle.
resolveImports :: Bundle Namespace -> PbMonad [ProtobufFile Namespace]
resolveImports b@(Bundle ps imap pmap) =
  mapM (resolvePkgImport b) [ pmap ! n | n <- ps ]

resolvePkgImport :: Bundle Namespace -> ProtobufFile Namespace -> PbMonad (ProtobufFile Namespace)
resolvePkgImport (Bundle _ imap pmap) (ProtobufFile pb qs names) = do
  global <- collectErrors
          $ foldM mergeNamespaces names
          [ ns | ProtobufFile _ _ ns <- [ pmap ! (imap ! i) | Import i <- pb ]
          ]
  return $ ProtobufFile pb qs global


----------------------------------------------------------------
-- * Stage 4. Resolve all names. All type names at this point are
--   converted into fully qualifie form.
resolveTypeNames :: ProtobufFile Namespace -> PbMonad (ProtobufFile Namespace)
resolveTypeNames p@(ProtobufFile _ _ global) =
  collectErrors $ transformBiM resolve p
  where
    -- Resolve type names in message
    resolve (Message name fields ns) = do
      f <- mapM (resolveField (Names global ns)) fields
      return $ Message name f ns
    -- Resolve type names in messag field
    resolveField ns (MessageField (Field m (UserType t) n tag o)) = do
      qt <- toTypename =<< resolveName ns t
      return $ MessageField $ Field m (UserType qt) n tag o
    resolveField _ x = return x

toTypename :: Qualified SomeName -> PbMonadE QIdentifier
toTypename (Qualified qs (MsgName  nm _)) = return $ FullQualId qs nm
toTypename (Qualified qs (EnumName nm  )) = return $ FullQualId qs nm
toTypename _ = throwError "Not a type name"
