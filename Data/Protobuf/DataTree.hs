-- | Tree which describe generated haskell data structures. It's then
-- converted to haskell-src-exts syntax tree and pretty-printed
--
-- * @required@ is translated into ordinary field
--
-- * @optional@ is translated into 'Maybe'
--
-- * @repeated@ could be either 'Seq' or 'Vector'
module Data.Protobuf.DataTree where

import qualified Data.Map as Map
import           Data.Map   (Map)



-- | Complete 
data DataTree = DataTree (Map String Module)

-- | Single module
data Module = Module



-- | Haskell declaration of protobuf Enum
data HsEnum = HsEnum TyName [(TyName,Integer)]
              deriving Show


-- | Haskell type or type constructor name. It must start from capital
--   letter
newtype TyName = TyName String
                 deriving Show
