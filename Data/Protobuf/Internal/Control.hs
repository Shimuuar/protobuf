-- |
-- Monads used for structuring computations.
module Data.Protobuf.Internal.Control (
    -- * Main monad
    PbMonad
  , runPbMonad
  , askContext
  , PbContext(..)
    -- * Monad for non-fatal error
  , PbMonadE
  , oops
  , collectErrors
  ) where


import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Error



----------------------------------------------------------------
-- Monads
----------------------------------------------------------------

-- | Monad for working with protobuf source tree.
type PbMonad =
  ErrorT String
   (ReaderT PbContext IO)

-- | Execute PbMonad
runPbMonad :: PbContext -> PbMonad a -> IO (Either String a)
runPbMonad cxt
  = flip runReaderT cxt
  . runErrorT

-- | Ask for context
askContext :: PbMonad PbContext
askContext = lift ask

-- | Context for protobuf compiler.
data PbContext = PbContext
  { includePaths :: [String]
    -- ^ List of include path
  }



----------------------------------------------------------------
-- Non-fatal errors
----------------------------------------------------------------

-- | Monad which allows to accumulate non-fatal errors
type PbMonadE =
  WriterT [String]
    PbMonad

-- | Non-fatal error
oops :: String -> PbMonadE()
oops = tell . (:[])

-- | Collect all non-fatal errors. If there are any raise an error.
collectErrors :: PbMonadE a -> PbMonad a
collectErrors m = do
  (x,errs) <- runWriterT m
  case errs of
    [] -> return x
    _  -> throwError $ unlines errs
