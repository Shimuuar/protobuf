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

-- * Stage 1. Remove package declarations and move package name into
--   ProtobufFile declaration.
removePackage :: ProtobufFile a -> PbMonad (ProtobufFile a)
removePackage (ProtobufFile pb _ a b) = do
  let (package,rest) = partition isPackage pb
      isPackage (Package _) = True
      isPackage _           = False
  p <- case package of
         []           -> return []
         [Package qs] -> return qs
         _            -> throwError "Multiple package declarations"
  return $ ProtobufFile rest p a b



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
traversePackage (Import i)    = return $ Import i
traversePackage (Package _)   = error "Impossible happended"
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


type NameBuilder = StateT Namespace PbMonadE

runNamespace a = flip runStateT emptyNamespace a

-- Add name into namespace
addToNamespace :: SomeName -> NameBuilder ()
addToNamespace n =
  put =<< lift . flip insertName n =<< get


----------------------------------------------------------------

-- * Stage 3. Resolve imports and put everything into top level
--   namespace
resolveImports :: Bundle Namespace -> PbMonad [ProtobufFile Namespace]
resolveImports b@(Bundle ps imap pmap) =
  mapM (resolvePkgImport b) 
  [ pmap ! n | n <- ps ]

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

-- * Stage 4. Resolve all names. All type names at this point are
--   converted into fully qualifie form
resolveNames :: ProtobufFile Namespace -> PbMonad (ProtobufFile Namespace)
resolveNames p@(ProtobufFile pb qs names (Global global)) = 
  collectErrors $ do
    pb' <- runResolver p $ mapM resolveTopLevel pb
    return $ ProtobufFile pb' qs names (Global global)


resolveTopLevel :: Protobuf Namespace -> NameResolve (Protobuf Namespace)
resolveTopLevel (TopMessage m) = TopMessage <$> resolveMessage m
resolveTopLevel x = return x

resolveMessage :: Message Namespace -> NameResolve (Message Namespace)
resolveMessage (Message nm fields names) = do
  f' <- descendName nm names $ mapM resolveField fields
  return $ Message nm f' names

resolveField :: MessageField Namespace -> NameResolve (MessageField Namespace)
resolveField (MessageField (Field m (UserType qid) nm tag o)) = do
  n <- lift . toTypename =<< resolveName qid
  return $ MessageField (Field m (UserType qid) nm tag o)
resolveField (Nested m) = do
  Nested <$> resolveMessage m
resolveField x = return x


data Names = Names Namespace [([Identifier], Namespace)]

type NameResolve = StateT Names PbMonadE

runResolver (ProtobufFile _ qs names (Global global)) = 
  flip evalStateT (Names global [(qs,names)])

descendName :: Identifier -> Namespace -> NameResolve a -> NameResolve a
descendName q ns action = do
  old@(Names global (stack@((qs , _):_))) <- get
  put $ Names global ((qs ++ [q], ns) : stack)
  x <- action
  x <$ put old

toTypename :: Qualified SomeName -> PbMonadE QIdentifier
toTypename (Qualified qs (MsgName  nm _)) = return $ FullQualId qs nm
toTypename (Qualified qs (EnumName nm  )) = return $ FullQualId qs nm
toTypename _ = throwError "Not a type name"

resolveName :: QIdentifier -> NameResolve (Qualified SomeName)
resolveName qid = do
  n <- get
  lift $ resolveNameWorker n qid

resolveNameWorker :: Names -> QIdentifier -> PbMonadE  (Qualified SomeName)
resolveNameWorker (Names global stack) (FullQualId qs n) =
  case findQualName global (Qualified qs n) of
    Just x  -> return x
    Nothing -> oops "Cannot find name" >> return (error "Impossible")  
resolveNameWorker (Names global []) (QualId qs n) =
  case findQualName global (Qualified qs n) of
    Just x  -> return x
    Nothing -> oops "Cannot find name" >> return (error "Impossible")  
resolveNameWorker (Names global ((qs,current):rest)) n@(QualId quals nm) =
  case findQualName current (Qualified quals nm) of
    Just x  -> return (addQualList qs x)
    Nothing -> resolveNameWorker (Names global rest) n
