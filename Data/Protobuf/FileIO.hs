{-# LANGUAGE RecordWildCards #-}
-- | Reading of protobuf files
module Data.Protobuf.FileIO (
    readPbFile
  , findImport
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Error

import System.Directory

import Data.Protobuf.Types
import Data.Protobuf.AST
import Data.Protobuf.Grammar.Parser
import Data.Protobuf.Grammar.Lexer


----------------------------------------------------------------
-- IO
----------------------------------------------------------------

-- | Read and parse protobuf file
readPbFile :: FilePath -> PbMonad [Protobuf]
readPbFile nm = do
  -- FIXME: catch IO errors
  src  <- liftIO $ readFile nm
  -- FIXME: catch lexing errors
  toks <- case scanTokens nm src of
            Left  err -> error err
            Right x   -> return x
  -- FIXME: catch parse errors
  return $ parseProtobuf toks

-- | Find canonicalized path for import
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
