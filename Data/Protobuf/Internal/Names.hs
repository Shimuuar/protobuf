{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Description of names for protocol buffers.
module Data.Protobuf.Internal.Names (
    -- * Identifiers
    -- ** Unqualified
    Identifier(..)
  , TagType(..)
  , TagField(..)
  , TagOption(..)
    -- ** Qualified
  , QualifiedId
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
    -- ** Lookup
  , findName
  , findQualName
  ) where

import Data.Data    (Typeable,Data)
import Data.Functor ((<$>))
import Data.Ord
import Data.Function
import qualified Data.Map         as Map
import           Data.Map           (Map)
import qualified Data.Foldable    as F

import Data.Protobuf.Internal.Control



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


type QualifiedId t = Qualified t (Identifier t)

-- | Qualified identifier
data Qualified t a = Qualified [Identifier t] a
                   deriving (Show,Eq,Ord,Typeable,Data)

-- | Add single qualifier
addQualifier :: Identifier t -> Qualified t a -> Qualified t a
addQualifier q (Qualified qs x) = Qualified (q:qs) x

-- | Add list of qualifiers.
addQualList :: [Identifier t] -> Qualified t a -> Qualified t a
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

-- | Find name in namespace
findName :: Namespace -> (Identifier TagType) -> Maybe SomeName
findName (Namespace ns) n = Map.lookup n ns

-- | Find qualified name in the namespace
findQualName :: Namespace
             -> QualifiedId TagType
             -> Maybe (Qualified TagType SomeName)
findQualName names (Qualified [] n)
  = Qualified [] <$> findName names n
findQualName names (Qualified (q:qs) n) = do
  nest <- findName names q
  case nest of
    MsgName _ ns -> addQualifier q <$> findQualName ns (Qualified qs n)
    PkgName _ ns -> addQualifier q <$> findQualName ns (Qualified qs n)
    _            -> Nothing
