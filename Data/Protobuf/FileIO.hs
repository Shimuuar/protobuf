{-# LANGUAGE RecordWildCards #-}
-- | Reading of protobuf files
module Data.Protobuf.FileIO (
    readProtobuf
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Error

import Data.Monoid
import qualified Data.Map as Map
import           Data.Map   (Map)

import System.Directory

import Data.Protobuf.Types
import Data.Protobuf.AST
import Data.Protobuf.Grammar.Parser
import Data.Protobuf.Grammar.Lexer



-- Path to file
data PbPath = ExactPath  FilePath -- Exact path
            | SearchPath FilePath -- Should be searched in the include dirs


-- | Read all files and load imports
readProtobuf :: [FilePath] -> PbMonad (Bundle ())
readProtobuf =
  foldM addFile (Bundle [] mempty mempty) . map ExactPath

-- Add file to bundle
addFile :: (Bundle ()) -> PbPath -> PbMonad (Bundle ())
addFile b (ExactPath nm) = do
  fname <- liftIO (canonicalizePath nm)
  b'    <- addAbsolutePath b fname
  return $ b' { processedFiles = fname : processedFiles b' }
addFile b@(Bundle{..}) (SearchPath nm)
  -- This import already loaded
  | nm `Map.member` importMap = return b
  -- Try to find it and store in the mapping
  | otherwise = do
      path <- findImport nm
      addAbsolutePath
        b { importMap = Map.insert nm path importMap }
        path

-- Find canonicalized path for import
findImport :: String -> PbMonad FilePath
findImport nm = search . includePaths =<< askContext
  where
    search []     = throwError $ "Import '" ++ nm ++ "' is not found!"
    search (d:ds) = do
      let name = d ++ "/" ++ nm
      exist <- liftIO $ doesFileExist name
      if exist
        then liftIO $ canonicalizePath name
        else search ds

-- Add file to map using absolute path
addAbsolutePath :: (Bundle ()) -> FilePath -> PbMonad (Bundle ())
addAbsolutePath b@(Bundle{..}) name
  -- File is already loaded
  | name `Map.member` importMap = return b
  -- Read file
  | otherwise                   = do
      pb@(ProtobufFile stmts _ _ _)  <- liftIO $ readPbFile name
      foldM addFile
        b { packageMap = Map.insert name pb packageMap }
        [SearchPath i | Import i <- stmts]

-- Read PB file from disk
readPbFile :: FilePath -> IO (ProtobufFile ())
readPbFile nm =
  (parseProtobuf . alexScanTokens) <$> readFile nm
