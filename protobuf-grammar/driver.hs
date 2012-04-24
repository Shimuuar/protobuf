{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
import Control.Monad
import Control.Monad.IO.Class

import Data.List
import qualified Data.Map as Map

import Text.Groom
import Text.PrettyPrint.ANSI.Leijen

import Language.Haskell.Exts.Pretty (prettyPrint)

import System.Directory
import System.Environment
import System.Console.GetOpt
import System.Exit

----------------------------------------------------------------
import Data.Protobuf.FileIO
import Data.Protobuf.AST            hiding (Option)
import Data.Protobuf.Types
import Data.Protobuf.DataTree
import Data.Protobuf.Transform
import Data.Protobuf.CodeGen

       
main :: IO ()
main = do
  pars <- parseArgs
  let cxt = PbContext { includePaths = "." : includes pars
                      }
  res <- runPbMonad cxt $ do
    -- Read protobuf files
    ast <- readProtobuf (fileList pars)
    when (dumpAST pars) $ 
      liftIO $ dumpBundle ast
    -- Check AST
    applyBundleM_ checkLabels ast
    -- Name mangling etc...
    let ast1 = applyBundle mangleNames 
             $ applyBundle sortLabels ast
    ast2 <- applyBundleM removePackage  ast1
    ast3 <- applyBundleM buildNamespace ast2
    pb1  <- resolveImports   ast3
    pb2  <- mapM resolveTypeNames pb1
    -- Convert to haskell
    DataTree hask <- toHaskellTree pb2
    -- Pretty print haskell code
    liftIO $ do 
      setCurrentDirectory (outputDir pars)
      mapM_ dump (Map.toList hask)
  -- Check for errors
  case res of
    Right _   -> return ()
    Left  err -> putStrLn err >> exitFailure



dump :: ([Identifier TagType], HsModule) -> IO ()
dump m@(map identifier -> qs, _) = do
  let dir  = intercalate "/" (init qs)
      file = last qs ++ ".hs"
  createDirectoryIfMissing True dir
  writeFile ("./" ++ dir ++ "/" ++ file) ((prettyPrint $ convert m) ++ "\n")

dumpPB :: Show a => ProtobufFile a -> IO ()
dumpPB pb = do
  putStrLn (groom pb)

dumpBundle :: Show a => Bundle a -> IO ()
dumpBundle (Bundle{..}) = do
  putStrLn "\n\n\n\n"
  let ln = putDoc $ red $ text "================================================================\n"
  ln
  mapM_ print $ Map.toList importMap
  ln
  forM_ (Map.toList packageMap) $ \(path, pb) -> do
    putDoc $
      blue (text "==== <<< ") <>
      green (text path) <>
      blue (text " >>> ================\n")
    dumpPB pb


----------------------------------------------------------------
-- Command line parameters
----------------------------------------------------------------

data Parameters = Parameters
  { fileList  :: [FilePath]
  , includes  :: [FilePath]
  , outputDir :: FilePath
  , dumpAST   :: Bool
  }

defaultParameters :: [FilePath] -> Parameters
defaultParameters fs = Parameters
  { fileList   = fs
  , includes   = []
  , outputDir  = "."
  , dumpAST    = False
  }

parseArgs :: IO Parameters
parseArgs = do
  args <- getArgs
  case getOpt Permute opts args of
    (xs,files,[]) -> return $ foldl (flip ($)) (defaultParameters files) xs
    (_,_,errs)    -> do mapM_ putStrLn errs
                        exitFailure
  where
    opts = 
      [ Option [] ["out_hs"] 
          (ReqArg (\arg p -> p { outputDir = arg }) "DIR")
          "Directory to put generate files to"
      , Option ['I'] []
          (ReqArg (\arg p -> p { includes = includes p ++ [arg] }) "DIR")
          "Include directory"
        -- Dump flags
      , Option [] ["dump-ast"]
           (NoArg $ \p -> p { dumpAST = True })
           "Dump AST"
      ]
