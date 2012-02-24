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

import qualified Data.Map as Map
import           Data.Map   (Map)

import Data.Protobuf.AST   (PrimType(..), FieldTag(..) )
import Data.Protobuf.Types (Qualified(..))

-- | complete 
data DataTree = DataTree (Map (Qualified String) HsModule)


-- | Haskell module. It contains single data type which corresponds
--   either to message or to enum.
data HsModule 
  = HsMessage TyName [HsField]
  | HsEnum    TyName [(TyName,Integer)]


-- | Single field of haskell message
data HsField 
  = HsField HsType String FieldTag

-- | Haskell type of field in message
data HsType 
  = HsSimple HsTypename         -- ^ Required data type
  | HsMaybe  HsTypename         -- ^ Optional data type
  | HsSeq    HsTypename         -- ^ Repeated data type as 'Seq'

-- | Name of type. 
data HsTypename
  = HsBuiltin  PrimType
  | HsUserType TyName



-- | Haskell type or type constructor name. It must start from capital
--   letter
newtype TyName = TyName String
                 deriving Show
