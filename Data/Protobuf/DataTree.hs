{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Tree which describe generated haskell data structures. It's then
-- converted to haskell-src-exts syntax tree and pretty-printed.
--
-- Each message and enum is put into separate module. This done in
-- order to avoid ovelapping of record field. Currently record system
-- of haskell is badly brocken.
--
-- * @required@ is translated into ordinary field
--
-- * @optional@ is translated into 'Maybe'
--
-- * @repeated@ could be either 'Seq' or 'Vector'
module Data.Protobuf.DataTree where

import Control.Monad.Error

import Data.Data
import Data.Monoid
import qualified Data.Map as Map
import           Data.Map   (Map)

import Data.Protobuf.AST
import Data.Protobuf.Names
import Data.Protobuf.Types (PbMonad)



-- | Complete list of modukes and data structures
newtype DataTree = DataTree (Map [Identifier TagType] HsModule)
                   deriving (Show,Data,Typeable)

-- | Haskell module. It contains single data type which corresponds
--   either to message or to enum.
data HsModule
  = HsMessage TyName [HsField]
  | HsEnum    TyName [(TyName,Integer)]
  deriving (Show,Data,Typeable)

-- | Single field of haskell message
data HsField
  = HsField HsType String FieldTag (Maybe OptionVal)
  deriving (Show,Data,Typeable)

-- | Haskell type of field in message
data HsType
  = HsReq    HsTypename         -- ^ Required data type
  | HsMaybe  HsTypename         -- ^ Optional data type
  | HsSeq    HsTypename Bool    -- ^ Repeated data type as 'Seq'. Flag indicate whether field is packed.
  deriving (Show,Data,Typeable)

-- | Name of type.
data HsTypename
  = HsBuiltin     PrimType
  | HsUserMessage (QualifiedId TagType)
  | HsUserEnum    (QualifiedId TagType)
  deriving (Show,Data,Typeable)



-- | Haskell type or type constructor name. It must start from capital
--   letter
newtype TyName = TyName String
                 deriving (Show,Data,Typeable)


----------------------------------------------------------------

data CollideMap k a = CollideMap [String] (Map k a)

runCollide :: CollideMap k a -> PbMonad (Map k a)
runCollide (CollideMap []   m) = return m
runCollide (CollideMap errs _) = throwError $ unlines $ "Collisions:" : errs

collide :: k -> v -> CollideMap k v
collide k v = CollideMap [] (Map.singleton k v)

instance (Show k, Ord k) => Monoid (CollideMap k a) where
  mempty = CollideMap mempty mempty
  mappend (CollideMap e1 m1) (CollideMap e2 m2) =
    Map.foldWithKey insert (CollideMap (e1++e2) m1) m2
    where
      insert k v (CollideMap errs m)
        | k `Map.member` m = CollideMap (show k : errs) m
        | otherwise        = CollideMap errs (Map.insert k v m)
