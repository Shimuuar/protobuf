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
removePackage (ProtobufFile pb _ a b) = do
  p <- case [ p | Package p <- pb ] of
         []   -> return []
         [qs] -> return qs
         _    -> throwError "Multiple package declarations"
  return $ ProtobufFile pb p a b



----------------------------------------------------------------
-- * Stage 2. Build and cache namespaces. During this stage name
--   collisions are discovered and repored as errors. Package
--   namespace is added to global namespace.
buildNamespace :: ProtobufFile a -> PbMonad (ProtobufFile Namespace)
buildNamespace (ProtobufFile pb qs _ _) =
  collectErrors $ do
    (pb',ns) <- runNamespace $ mapM traversePackage pb
    return $ ProtobufFile pb' qs ns (Global $ foldr packageNamespace ns qs)


-- Traverse top level definitions and cache namespaces
traversePackage :: Protobuf n -> NameBuilder (Protobuf Namespace)
traversePackage (TopEnum    e) =
  TopEnum e <$ collectEnumNames e
traversePackage (TopMessage msg) = do
  TopMessage <$> collectMessageNames msg
traversePackage (Import  i)   = return $ Import  i
traversePackage (Package p)   = return $ Package p
traversePackage (Extend _ _)  = throwError "Extensions are not supported"
traversePackage (TopOption o) = return $ TopOption o

-- Build namespace for message
collectMessageNames :: Message e -> NameBuilder (Message Namespace)
collectMessageNames (Message name flds _) = do
  (fields,names) <- lift $ runNamespace $ mapM collectFieldNames flds
  addToNamespace (MsgName name names)
  return $ Message name fields names

-- Collect names of fields
collectFieldNames :: MessageField e -> NameBuilder (MessageField Namespace)
collectFieldNames (MessageField f@(Field _ _ n _ _)) =
  MessageField f <$ addToNamespace (FieldName n)
collectFieldNames (MessageEnum e) = MessageEnum e <$ collectEnumNames e
collectFieldNames (Nested m)      = Nested <$> collectMessageNames m
collectFieldNames  MsgExtend      = return  MsgExtend
collectFieldNames (Extensions e)  = return (Extensions e)
collectFieldNames (MsgOption  o)  = return (MsgOption o)

-- Collect all names from into namespace
collectEnumNames :: EnumDecl -> NameBuilder ()
collectEnumNames (EnumDecl nm flds) = do
  addToNamespace (EnumName nm)
  mapM_ addToNamespace [EnumElem n | EnumField n _ <- flds]

-- Monad for accumulation of namespaces
type NameBuilder = StateT Namespace PbMonadE

-- Evaluate namespace builder
runNamespace :: StateT Namespace m a -> m (a, Namespace)
runNamespace = flip runStateT emptyNamespace

-- Add name into namespace
addToNamespace :: SomeName -> NameBuilder ()
addToNamespace n =
  put =<< lift . flip insertName n =<< get



----------------------------------------------------------------
-- * Stage 3. Resolve imports and build global namespace. Name clashes
--   in import are discovered during this stage. After this stage each
--   protobuf file is self containted so we can discard bundle.
resolveImports :: Bundle Namespace -> PbMonad [ProtobufFile Namespace]
resolveImports b@(Bundle ps imap pmap) =
  mapM (resolvePkgImport b) [ pmap ! n | n <- ps ]

resolvePkgImport :: Bundle Namespace -> ProtobufFile Namespace -> PbMonad (ProtobufFile Namespace)
resolvePkgImport (Bundle _ imap pmap) (ProtobufFile pb quals names (Global ns)) = do
  let isImport (Import _) = True
      isImport _          = False
      (imports,pb') = partition isImport pb
  global <- collectErrors
          $ foldM mergeNamespaces ns
          [ foldr packageNamespace pkgNames qs
          | ProtobufFile _ qs pkgNames _ <- [ pmap ! (imap ! i) | Import i <- imports ]
          ]
  return $ ProtobufFile pb' quals names (Global global)


----------------------------------------------------------------
-- * Stage 4. Put global namespace everywhere in the AST
globalNamespace :: ProtobufFile Namespace -> ProtobufFile Names
globalNamespace (ProtobufFile pb qs ns (Global glob)) =
  ProtobufFile
    (traverseGlobal global <$> pb)
    qs global (Global global)
  where
    global = Names glob []

traverseGlobal :: Names -> Protobuf a -> Protobuf Names
traverseGlobal _ (TopEnum    e)   = TopEnum e
traverseGlobal g (TopMessage msg) = TopMessage $ traverseMessage g msg
traverseGlobal _ (Import  i)      = Import i
traverseGlobal _ (Package p)      = Package p
traverseGlobal _ (Extend a b)     = error "Extend is not supported"
traverseGlobal _ (TopOption o)    = TopOption o

traverseMessage :: Names -> Message a -> Message Names
traverseMessage g (Message name fields _) =
  Message name (traverseField g' <$> fields) g'
  where
    g' = nameDown name g

traverseField :: Names -> MessageField t -> MessageField Names
traverseField _ (MessageField f) = MessageField f
traverseField _ (MessageEnum e)  = MessageEnum e
traverseField g (Nested m)       = Nested $ traverseMessage g m
traverseField _  MsgExtend       = MsgExtend
traverseField _ (Extensions e)   = Extensions e
traverseField _ (MsgOption  o)   = MsgOption o



----------------------------------------------------------------
-- * Stage 4. Resolve all names. All type names at this point are
--   converted into fully qualifie form.
resolveTypeNames :: ProtobufFile Names -> PbMonad (ProtobufFile Names)
resolveTypeNames p =
  collectErrors $ transformBiM resolve p
  where
    resolve (Message name fields ns) = do
      f <- mapM (resolveField ns) fields
      return $ Message name f ns
    resolveField ns (MessageField (Field m (UserType t) n tag o)) = do
      qt <- toTypename =<< resolveName ns t
      return $ MessageField $ Field m (UserType qt) n tag o
    resolveField _ x = return x

toTypename :: Qualified SomeName -> PbMonadE QIdentifier
toTypename (Qualified qs (MsgName  nm _)) = return $ FullQualId qs nm
toTypename (Qualified qs (EnumName nm  )) = return $ FullQualId qs nm
toTypename _ = throwError "Not a type name"
