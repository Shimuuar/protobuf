{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Description of names for protocol buffers.
module Data.Protobuf.Names (
    -- * Identifiers
    -- ** Unqualified
    Identifier(..)
  , TagType(..)
  , TagField(..)
  , TagOption(..)
    -- ** Qualified
  , Qualified(..)
  , addQualifier
  , addQualList
    -- * Namespaces
  , Namespace
  , SomeName(..)
    -- ** Constructors
  , emptyNamespace
  , packageNamespace
    -- ** Modification
  , insertName
  , mergeNamespaces
  ) where

import Data.Data  (Typeable,Data)
import Data.Ord
import Data.Function
import qualified Data.Map         as Map
import           Data.Map           (Map)
import qualified Data.Foldable    as F
import qualified Data.Traversable as T

import Data.Protobuf.Types


----------------------------------------------------------------
-- Identifiers
----------------------------------------------------------------

-- | Simple unqualified identifier. Phantom parameter is used to
--   distinguish between different names
newtype Identifier t = Identifier { identifier :: String }
                   deriving (Typeable,Data,Eq,Ord)
instance Show (Identifier t) where
  show = show . identifier

-- | Tag for name of data type (message or enum).
data TagType   = TagType    deriving (Typeable,Data)
-- | Tag for name of message field.
data TagField  = TagField   deriving (Typeable,Data)
-- | Tag for name of option.
data TagOption = TagOption  deriving (Typeable,Data)


-- | Qualified identifier
data Qualified t = Qualified [Identifier t] (Identifier t)
                 deriving (Show,Eq,Ord,Typeable,Data)

-- | Add single qualifier
addQualifier :: Identifier t -> Qualified t -> Qualified t
addQualifier q (Qualified qs x) = Qualified (q:qs) x

-- | Add list of qualifiers.
addQualList :: [Identifier t] -> Qualified t -> Qualified t
addQualList q (Qualified qs x) = Qualified (q ++ qs) x



----------------------------------------------------------------
-- Namespace
----------------------------------------------------------------

-- | Complete namespace description.
newtype Namespace = Namespace (Map (Identifier TagType) SomeName)
                  deriving (Typeable,Data)

-- | Single name in a set. Messages and packages have inner
--   namespaces.
data SomeName
  = MsgName   (Identifier TagType) Namespace
  | PkgName   (Identifier TagType) Namespace
  | FieldName (Identifier TagType)
  | EnumName  (Identifier TagType)
  | EnumElem  (Identifier TagType)
  deriving (Show,Typeable,Data)

nameToId :: SomeName -> (Identifier TagType)
nameToId (MsgName   n _) = n
nameToId (PkgName   n _) = n
nameToId (FieldName n  ) = n
nameToId (EnumName  n  ) = n
nameToId (EnumElem  n  ) = n

instance Eq SomeName where
  (==) = (==) `on` nameToId
instance Ord SomeName where
  compare = comparing nameToId


instance Show Namespace where
  show (Namespace m) = show $ Map.elems m


----------------------------------------------------------------
-- Constructors
----------------------------------------------------------------

-- | Empty namespace
emptyNamespace :: Namespace
emptyNamespace = Namespace Map.empty

-- | Put namespace into package
packageNamespace :: Identifier TagType -- ^ Unqualified package name
                 -> Namespace          -- ^ Package namespace
                 -> Namespace
packageNamespace pkg ns
  = Namespace $ Map.singleton pkg $ PkgName pkg ns


-- | Insert name into set while checking for duplicates
insertName :: Namespace         -- ^ Namespace
           -> SomeName          -- ^ Name to insert
           -> PbMonadE Namespace
-- We need to treat insertion of package names specially since we need
-- to merge package names
insertName (Namespace ns) pkg@(PkgName n ns') =
  case Map.lookup n ns of
    Nothing            -> return $ Namespace $ Map.insert n pkg ns
    Just (PkgName _ m) -> do combined <- mergeNamespaces m ns'
                             return $ Namespace $ Map.insert n (PkgName n combined) ns
    _                  -> do oops $ "Duplicate name: " ++ identifier n
                             return $ Namespace ns
-- Otherwise insert name if there no collisions
insertName (Namespace ns) name
  | n `Map.member` ns = do oops $ "Duplicate name: " ++ identifier n
                             ++ "\n" ++ show ns
                           return (Namespace ns)
  | otherwise         = return $ Namespace $ Map.insert n name ns
  where
    n = nameToId name


-- | Merge two namespaces and collect error during that.
mergeNamespaces :: Namespace -> Namespace -> PbMonadE Namespace
mergeNamespaces namespace (Namespace ns2)
  = F.foldlM insertName namespace ns2



----------------------------------------------------------------
-- Names
----------------------------------------------------------------

-- -- | Set of namespaces. First parameter is global namespace and second
-- --   is current path into namespace
-- data Names = Names Namespace [(Identifier TagType)]
--            deriving (Show,Typeable,Data)

-- nameDown :: Identifier TagType -> Names -> Names
-- nameDown n (Names global path) = Names global (path ++ [n])

-- resolveName :: Names -> QIdentifier -> PbMonadE (Qualified TagType SomeName)
-- resolveName (Names global _ ) (FullQualId qs n) = resolveNameWorker global qs n
-- resolveName (Names global []) (QualId     qs n) = resolveNameWorker global qs n
-- resolveName (Names global path) name@(QualId qs n) =
--   case findQualName global (Qualified (path ++ qs) n) of
--     Just x  -> return x
--     Nothing -> resolveName (Names global (init path)) name

-- resolveNameWorker :: Namespace -> [(Identifier TagType)] -> (Identifier TagType) -> PbMonadE (Qualified TagType SomeName)
-- resolveNameWorker namespace qs n =
--   case findQualName namespace (Qualified qs n) of
--     Just x  -> return x
--     Nothing -> do oops $ "Cannot find name" ++ show (Qualified qs n)
--                   return (Qualified [] $ EnumName $ Identifier "DUMMY")
