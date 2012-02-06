-- | Reading of protobuf files
module Data.Protobuf.FileIO (
    readProtobuf
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

import Data.Monoid
import qualified Data.Map as Map
import           Data.Map   (Map)

import System.Directory

import Data.Protobuf.Types
import Data.Protobuf.AST
import Data.Protobuf.Grammar.Parser
import Data.Protobuf.Grammar.Lexer



-- | Path to file
data PbPath = ExactPath  FilePath -- ^ Exact path
            | SearchPath FilePath -- ^ Should be searched in the include dirs

-- | Read all files and load imports
readProtobuf :: [String] -> PbMonad FileMap
readProtobuf = foldM addFile mempty . map ExactPath

-- | Find canonicalized path for import
findImport :: String -> PbMonad FilePath
findImport nm = search . includePaths =<< askContext
  where
    search []     = fail $ "Import '" ++ nm ++ "' is not found!"
    search (d:ds) = do
      let name = d ++ "/" ++ nm ++ ".proto"
      exist <- liftIO $ doesFileExist name 
      if exist
        then liftIO $ canonicalizePath name
        else search ds
  
-- | Read PB file from disk
readPbFile :: FilePath -> IO [Protobuf]
readPbFile nm = do
  (parseProtobuf . alexScanTokens) <$> readFile nm

-- | Add file to file map
addFile impMap (ExactPath nm) = do
  addAbsolutePath impMap =<< liftIO (canonicalizePath nm)
addFile impMap (SearchPath nm) = do
  addAbsolutePath impMap =<< findImport nm

-- | Add file to map using absolute path
addAbsolutePath impMap name
  | name `Map.member` impMap = return impMap
  | otherwise                = do 
      pb <- liftIO $ readPbFile name
      foldM addFile (Map.insert name pb impMap) [SearchPath i | Import i <- pb]

