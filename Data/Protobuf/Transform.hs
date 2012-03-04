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
import Data.Char
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


import Debug.Trace



----------------------------------------------------------------
-- Validation
----------------------------------------------------------------

-- | Check that there are no duplicate labels
checkLabels :: Data a => ProtobufFile a -> PbMonad ()
checkLabels pb = collectErrors $ do
  mapM_ checkMessage [ fs | Message  _ fs _ <- universeBi pb ]
  mapM_ checkEnum    [ fs | EnumDecl _ fs _ <- universeBi pb ]
  where
    checkMessage fs = 
      when (labels /= nub labels) $
        oops "Duplicate labels"
      where labels = [ i | MessageField (Field _ _ _ (FieldTag i) _) <- fs ]
    checkEnum fs = 
      when (labels /= nub labels) $
        oops "Duplicate labels"
      where labels = [ i | EnumField _ i <- fs ]



----------------------------------------------------------------
-- Normalization
----------------------------------------------------------------

----------------------------------------------------------------
-- * Stage 1. Mangle all names. 
mangleNames :: Data a => ProtobufFile a -> ProtobufFile a
mangleNames 
  = transformBi mangleFieldName
  . transformBi mangleTypeName

-- Convert type/constructor/package name to upper case
mangleTypeName :: Identifier -> Identifier
mangleTypeName x = traceShow (x,x') x'
  where x' = mangleTypeName' x

mangleTypeName' :: Identifier -> Identifier
mangleTypeName' (Identifier (c:cs)) = Identifier $ toUpper c : cs
mangleTypeName' (Identifier "")     = error "Impossible happened: invalid field identifier"

-- Only field names in messages should start from lower case
mangleFieldName :: IdentifierF -> IdentifierF
mangleFieldName (IdentifierF (c:cs)) = IdentifierF $ toLower c : cs
mangleFieldName (IdentifierF "")     = error "Impossible happened: invalid field identifier"

----------------------------------------------------------------
-- * Stage 2. Remove package declarations and move package name into
--   ProtobufFile declaration.
removePackage :: ProtobufFile a -> PbMonad (ProtobufFile a)
removePackage (ProtobufFile pb _ x) = do
  p <- case [ p | Package p <- pb ] of
         []   -> return []
         [qs] -> return qs
         _    -> throwError "Multiple package declarations"
  return $ ProtobufFile pb p x


----------------------------------------------------------------
-- * Stage 3. Build and cache namespaces. During this stage name
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
  f <$ addName (FieldName $ Identifier $ identifierF n)
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
-- * Stage 4. Resolve imports and build global namespace. Name clashes
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
-- * Stage 5. Resolve all names. All type names at this point are
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
    resolveField ns (MessageField (Field m (SomeType t) n tag o)) = do
      qt <- toTypename =<< resolveName ns t
      return $ MessageField $ Field m qt n tag o
    resolveField _ x = return x

toTypename :: Qualified SomeName -> PbMonadE Type
toTypename (Qualified qs (MsgName  nm _)) = return $ MsgType  $ FullQualId qs nm
toTypename (Qualified qs (EnumName nm  )) = return $ EnumType $ FullQualId qs nm
toTypename _ = throwError "Not a type name"



----------------------------------------------------------------
-- * Stage 6. Convert AST to haskell representation
toHaskellTree :: [ProtobufFile Namespace] -> PbMonad DataTree
toHaskellTree pb =
  DataTree <$> runCollide (mconcat decls)
  where
    decls =  [ enumToHask    e | e <- universeBi pb ]
          ++ [ messageToHask m | m <- universeBi pb ]

-- Convert enumeration to haskell
enumToHask :: EnumDecl -> CollideMap [Identifier] HsModule
enumToHask (EnumDecl iname@(Identifier name) fields qs) =
  collide (qs ++ [iname]) $ HsEnum (TyName name)
  [ (TyName n, i) | EnumField (Identifier n) i <- fields ]

-- Convert message to haskell
messageToHask :: Message -> CollideMap [Identifier] HsModule
messageToHask (Message (Identifier name) fields qs) =
  collide qs $ HsMessage (TyName name) [fieldToHask f | MessageField f <- fields]

-- Convert field to haskell
fieldToHask :: Field -> HsField
fieldToHask (Field m t n tag _) =
  HsField (con hsTy) (identifierF n) tag
  where
    -- Haskell field outer type
    con = case m of Required -> HsReq
                    Optional -> HsMaybe
                    Repeated -> HsSeq
    -- Haskell field inner type
    hsTy = case t of
      BaseType  ty                 -> HsBuiltin  ty
      (MsgType  (FullQualId qs n)) -> HsUserMessage (Qualified qs n)
      (EnumType (FullQualId qs n)) -> HsUserEnum    (Qualified qs n)
      _ -> error "Impossible happened: name isn't fully qualifed"

