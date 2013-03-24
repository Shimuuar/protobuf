{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
-- | Transofrmation of protobug AST
module Data.Protobuf.Transform (
    -- * Loading
    Bundle(..)
  , loadPbFiles
    -- * Validation
  , checkLabels
    -- * Transformations
  , sortLabels
  , ProtobufFile(..)
  , buildNamespace
  , mergeImports
  , resolveTypeNames
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Error

import qualified Data.Map         as Map
import           Data.Map           (Map,(!))
import Data.Data                 (Data,Typeable)
import Data.Ord
import Data.List
import Data.Monoid
import qualified Data.Foldable    as F
import qualified Data.Traversable as T

import Data.Generics.Uniplate.Data

import Data.Protobuf.AST
import Data.Protobuf.Names
import Data.Protobuf.Types
import Data.Protobuf.DataTree
import Data.Protobuf.FileIO



----------------------------------------------------------------
-- Import resolution
----------------------------------------------------------------

-- | This data type contain all protobuf files which should be
--   processed.
data Bundle a = Bundle
  { processedFiles :: [FilePath]
    -- ^ Pathes to the files for which code should be generated
  , importMap  :: DMap String FilePath a
    -- ^ Maps import strings to the pathes in the file system and
    --   latter to the loaded sources. 
  }
  deriving (Typeable,Functor,F.Foldable,T.Traversable)


-- | Read all protobuf files and build map of all imports. 
loadPbFiles :: [FilePath] -> PbMonad (Bundle [Protobuf])
loadPbFiles fnames = do
  -- Read and normalize pathes at the same time
  (norm,dmap) <- flip runStateT emptyDMap
               $ mapM loadByPath fnames
  return $ Bundle norm dmap


type FileReader = StateT (DMap String FilePath [Protobuf]) PbMonad

-- Load file by path and 
loadByPath :: FilePath -> FileReader FilePath
loadByPath path = do
  nm <- lift $ normalizePath path
  pb <- lift $ readPbFile nm
  modify $ insertDMap2 nm pb
  mapM_ loadImports pb
  return nm

-- Load imported protobuf files recursively
loadImports :: Protobuf -> FileReader ()
loadImports (Import str) = do
  (r,dmap) <- lift . insertDMapM findImport readPbFile str =<< get
  put dmap
  case r of
    Nothing -> return ()
    Just ps -> mapM_ loadImports ps
loadImports _ = return ()
  


----------------------------------------------------------------
-- Validation
----------------------------------------------------------------

-- | Check that there are no duplicate label numbers.
checkLabels :: [Protobuf] -> PbMonad ()
checkLabels pb = collectErrors $ do
  mapM_ checkMessage [ fs | Message  _ fs _ <- universeBi pb ]
  mapM_ checkEnum    [ fs | EnumDecl _ fs _ <- universeBi pb ]
  mapM_ checkFieldTag $ universeBi pb
  where
    -- Check for duplicate tags in message
    checkMessage fs = 
      when (labels /= nub labels) $
        oops "Duplicate label number"
      where labels = [ i | MessageField (Field _ _ _ (FieldTag i) _) <- fs ]
    -- Check for duplicate tags in enumerations
    checkEnum fs = 
      when (labels /= nub labels) $
        oops "Duplicate label number"
      where labels = [ i | EnumField _ i <- fs ]
    -- Check that tags are in range
    checkFieldTag (FieldTag n)
      | n < 1 || n > (2^(29::Int) - 1) = oops "Field tag is outside of range"
      | n >= 19000 && n <= 19999       = oops "Field tag is in reserved range"
      | otherwise                      = return ()



----------------------------------------------------------------
-- Normalization
----------------------------------------------------------------

-- | Sort fields in message declarations by tag.
sortLabels :: [Protobuf] -> [Protobuf]
sortLabels = transformBi (sortBy $ comparing tag)
  where
    tag (MessageField (Field _ _ _ (FieldTag t) _)) = t
    tag _                                           = -1



----------------------------------------------------------------
-- Namespace building
----------------------------------------------------------------

-- | Protocol buffer file. In addition of the 
data ProtobufFile = ProtobufFile
  { protobufFile      :: [Protobuf]
    -- ^ List of all protocol buffer statements
  , protobufPackage   :: [Identifier TagType]
    -- ^ Package name
  , protobufNamespace :: Namespace
    -- ^ All names in the package
  }
  deriving (Show,Typeable,Data)

-- | This function does two things at once.
--
--   1. It builds set of all names in the current module which will be
--      later used in the name resolution.
--
--   2. All names of messages and enums declared in the module are
--      fully qualified with module name.
buildNamespace :: [Protobuf] -> PbMonad ProtobufFile
buildNamespace pb = do
  -- Find out package name
  pkg <- case [ p | Package p <- pb ] of
           []               -> return []
           [Qualified qs q] -> return (qs ++ [q])
           _                -> throwError "Multiple package declarations"
  -- Build namespace
  (pb',names) <- collectErrors $ do
    runNamespace $ mapM (collectPackageNames pkg) pb
  return $ ProtobufFile pb' pkg (foldr packageNamespace names pkg)


-- Collect all names in package
collectPackageNames :: [Identifier TagType] -> Protobuf -> NameCollector Protobuf
collectPackageNames path (TopMessage m) =
  TopMessage <$> collectMessageNames path m
collectPackageNames path (TopEnum    e) =
  TopEnum    <$> collectEnumNames    path e
collectPackageNames _ x = return x

-- Get namespace for a message
collectMessageNames :: [Identifier TagType] -> Message -> NameCollector Message
collectMessageNames path (Message name fields _) = do
  let path' = path ++ [name]
  (fs,ns) <- lift $ runNamespace $ mapM (collectFieldNames path') fields
  addName $ MsgName name ns
  return  $ Message name fs path'

-- Get namespace for an enum
collectEnumNames :: [Identifier TagType] -> EnumDecl -> NameCollector EnumDecl
collectEnumNames path (EnumDecl name fields _) = do
  addName (EnumName name)
  mapM_ addName [ FieldName n | EnumField n _ <- fields]
  return $ EnumDecl name fields path

-- Collect names from the fields
collectFieldNames :: [Identifier TagType] -> MessageField -> NameCollector MessageField
collectFieldNames _ f@(MessageField (Field _ _ n _ _)) =
  f <$ addName (FieldName $ Identifier $ identifier n)
collectFieldNames path (Nested m) =
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
-- Merge imports into namespace
----------------------------------------------------------------

mergeImports :: DMap String FilePath ProtobufFile -> ProtobufFile -> PbMonad ProtobufFile
mergeImports impMap (ProtobufFile ps pkg names) = do
  names' <- collectErrors
          $ foldM mergeNamespaces names [ loadImport i | Import i <- ps ]
  return $ ProtobufFile ps pkg names'
  where
    loadImport str =
      case str `lookupDMap` impMap of
        Just p  -> protobufNamespace p
        Nothing -> error "Impossible happened: import could not be resolved"


----------------------------------------------------------------
-- Name resolution
----------------------------------------------------------------


-- | Resolves all type names.
--
--   Postconditions:
--
--   * All type names are fully qualified
--   * There is no 'SomeName' constructors 
resolveTypeNames :: ProtobufFile -> PbMonad ProtobufFile
-- Name resolution is local to message. Since by this time fully
-- qualified message name is known we know scope for message and also
-- we have all available names.
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


-- | Set of namespaces. First parameter is global namespace and second
--   is current path into namespace
data Names = Names Namespace [(Identifier TagType)]
           deriving (Show,Typeable,Data)

resolveName :: Names -> QIdentifier -> PbMonadE (Qualified TagType SomeName)
resolveName (Names global _ ) (FullQualId q) = resolveNameWorker global q
resolveName (Names global []) (QualId     q) = resolveNameWorker global q
resolveName (Names global path) name@(QualId qname) =
  case findQualName global $ addQualList path qname of
    Just x  -> return x
    Nothing -> resolveName (Names global (init path)) name

resolveNameWorker :: Namespace
                  -> QualifiedId TagType
                  -> PbMonadE (Qualified TagType SomeName)
resolveNameWorker namespace qname =
  case findQualName namespace qname of
    Just x  -> return x
    Nothing -> do oops $ "Cannot find name" ++ show qname
                  return $ Qualified [] $ EnumName $ Identifier "<<<DUMMY>>>"

toTypename :: Qualified TagType SomeName -> PbMonadE Type
toTypename (Qualified qs (MsgName  nm _)) = return $ MsgType  $ FullQualId $ Qualified qs nm
toTypename (Qualified qs (EnumName nm  )) = return $ EnumType $ FullQualId $ Qualified qs nm
toTypename _ = throwError "Not a type name"
