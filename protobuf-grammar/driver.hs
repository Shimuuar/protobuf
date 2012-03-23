{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Error
import Data.List
import qualified Data.Map as Map
import Text.Groom
import Text.PrettyPrint.ANSI.Leijen
import Language.Haskell.Exts.Pretty (prettyPrint)
import System.Directory
import System.Environment
----------------------------------------------------------------
import Data.Protobuf.FileIO
import Data.Protobuf.AST
import Data.Protobuf.Types
import Data.Protobuf.DataTree
import Data.Protobuf.Transform
import Data.Protobuf.CodeGen
----------------------------------------------------------------

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


runPbMonad m
  = flip runReaderT (PbContext ["."])
  $ runErrorT
  $ m

go :: [String] -> IO ()
go files = do
  x <- runPbMonad $ do
    -- Read
    s0 <- readProtobuf files
    applyBundleM_ checkLabels s0
    liftIO $ dumpBundle s0
    --
    let s0' = applyBundle mangleNames 
            $ applyBundle sortLabels s0
    liftIO $ dumpBundle s0'
    -- Stage 2
    s1 <- applyBundleM removePackage s0'
    liftIO $ dumpBundle s1
    -- Stage 3
    s2 <- applyBundleM buildNamespace s1
    liftIO $ dumpBundle s2
    -- Stage 4
    s3 <- resolveImports s2
    liftIO $ do
      putDoc $ red $ text $ "\n\n==== STAGE 3 ================\n"
      mapM dumpPB s3
    -- Stage 5
    s4 <- mapM resolveTypeNames s3
    liftIO $ do
      putDoc $ red $ text $ "\n\n==== STAGE 4 ================\n"
      mapM dumpPB s4
    -- Convert to haskell
    s5 <- toHaskellTree s4
    liftIO $ print s5
    liftIO $ do
      putDoc $ red $ text $ "\n\n==== HASKELL ================\n"
      let DataTree q = s5
      forM_ (Map.toList q) $ \(n,aa) -> do
        putDoc $ blue $ text $ "----------------------------------------\n"
        putDoc $ blue $ text $ show n
        putStrLn ""
        putStrLn $ groom aa
    -- P-print haskell code
    liftIO $ do
      putDoc $ red $ text $ "\n\n==== HASKELL ================\n"
      let DataTree q = s5
      forM_ (Map.toList q) $ \zz -> do
        putDoc $ red $ text $ "----------------------------------------\n"
        (putStrLn . prettyPrint . convert) zz
    -- ----------------
    liftIO $ do
      -- setCurrentDirectory "gen"
      let DataTree q = s5
      mapM_ dump (Map.toList q)
      -- setCurrentDirectory ".."
    return ()
  case x of
    Left err -> do
      putDoc $ red $ text "----------------------------------------------------------------\n"
      putDoc $ red $ text err
    Right x  -> return ()
  return ()


main :: IO ()
main = go =<< getArgs

----------------------------------------------------------------


dump :: ([Identifier TagType], HsModule) -> IO ()
dump m@(map identifier -> qs, _) = do
  let dir  = intercalate "/" (init qs)
      file = last qs ++ ".hs"
  createDirectoryIfMissing True dir
  writeFile ("./" ++ dir ++ "/" ++ file) ((prettyPrint $ convert m) ++ "\n")
