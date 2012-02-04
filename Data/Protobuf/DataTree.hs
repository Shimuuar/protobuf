-- | Tree which describe generated haskell data structures. It's then
-- converted to haskell-src-exts syntax tree and pretty-printed
module Data.Protobuf.DataTree where

import qualified Data.Map as Map
import           Data.Map   (Map)



-- | Complete 
data DataTree = DataTree (Map String Module)

-- | Single module
data Module = Module
