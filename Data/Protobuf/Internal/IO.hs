{-# LANGUAGE RecordWildCards #-}
-- | Reading of protobuf files
module Data.Protobuf.Internal.IO (
    readPbFile
  , findImport
  , normalizePath
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Error

import System.Directory

import Data.Protobuf.Internal.Types
import Data.Protobuf.Internal.AST
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
        then normalizePath name
        else search ds

-- | Normalize path to file
normalizePath :: FilePath -> PbMonad FilePath
normalizePath = liftIO . canonicalizePath
