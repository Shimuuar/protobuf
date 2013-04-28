{-# LANGUAGE DeriveDataTypeable #-}
-- |
module Data.Protobuf (
    -- * Data types
    QName(..)
  , PbDatatype(..)
  , PbField(..)
  , PbType(..)
  , PbOption(..)
  , OptionVal(..)
    -- * Reading of protobuf
  , loadProtobuf
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe

import Data.Functor ((<$>))
import Data.Maybe   (fromMaybe)
import Data.Data    (Data,Typeable)
import qualified Data.Foldable    as F
import qualified Data.Traversable as T
import Data.Generics.Uniplate.Data

import Data.Protobuf.Internal.AST
import Data.Protobuf.Internal.Names
import Data.Protobuf.Internal.Control
import Data.Protobuf.Internal.Types
import Data.Protobuf.Internal.Transform



----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Qualified name
data QName = QName [String] String
           deriving (Show,Eq,Typeable,Data)

-- | Data type declared in the .proto file. It could be either message
--   or enumeration.
data PbDatatype
  = PbMessage QName [PbField]
  | PbEnum    QName [(Integer,String)]
  deriving (Show,Eq,Typeable,Data)

-- | Field of message.
data PbField = PbField Modifier PbType String Integer [PbOption]
             deriving (Show,Eq,Typeable,Data)

-- | Type of field
data PbType
  = TyMessage QName
  | TyEnum    QName
  | TyPrim    PrimType
  deriving (Show,Eq,Typeable,Data)

-- | Supported protobuf options
data PbOption
  = OptDefault OptionVal
  | OptPacked
  deriving (Show,Eq,Typeable,Data)



----------------------------------------------------------------
-- Reading protobuf source
----------------------------------------------------------------

-- | Load all protobuf files.
loadProtobuf :: [String]                   -- ^ Search path for includes
             -> [FilePath]                 -- ^ Files to load
             -> IO (Either String [PbDatatype])
loadProtobuf includes srcs = runPbMonad (PbContext includes) $ do
  -- 1. Actually load files
  bundle <- loadPbFiles srcs
  -- 2. Check sources for obvious error
  F.mapM_ checkLabels bundle
  -- 3. Build namespaces
  Bundle pathes dmap <- T.traverse buildNamespace
                      $ fmap sortLabels bundle
  let toPB path = case lookupDMap2 path dmap of
                    Just x  -> x
                    Nothing -> error "Internal error in protobuf"
      files = map toPB pathes
  -- 4. Result
  res <-  mapM resolveTypeNames
      =<< mapM (mergeImports dmap) files
  -- Extract data from AST
  collectErrors $ concat <$> mapM extractData res



----------------------------------------------------------------
-- Worker functions
----------------------------------------------------------------

extractData :: ProtobufFile -> PbMonadE [PbDatatype]
extractData pb = do
  msgs  <- mapM cnvMessage $ universeBi $ protobufFile pb
  enums <- return $ map cnvEnum    $ universeBi $ protobufFile pb
  return $ msgs ++ enums
  where

-- Extract message
cnvMessage :: Message -> PbMonadE PbDatatype
cnvMessage (Message nm fields)
  = PbMessage (toQName nm) <$> (concat <$> mapM cnvField fields)
  where
    cnvField (MessageField (Field modif ty name (FieldTag tag) opts)) = do
      optPacked  <- optionPacked  modif fType opts
      optDefault <- optionDefault modif fType opts
      return [PbField modif fType (identifier name) tag (optPacked ++ optDefault)]
      where
        -- Type of field
        fType = case ty of
                  BaseType p -> TyPrim p
                  EnumType q -> TyEnum    $ qname q
                  MsgType  q -> TyMessage $ qname q
                  _          -> error "Internal error: unresolved name"
        -- Check for default option
    cnvField _ = return []


-- Extract enums
cnvEnum :: EnumDecl -> PbDatatype
cnvEnum (EnumDecl nm fields)
  = PbEnum (toQName nm)
      [ (i,name) | EnumField (Identifier name) i <- fields]



----------------------------------------------------------------
-- Extract option "packed"
optionPacked :: Modifier -> PbType -> [Option] -> PbMonadE [PbOption]
optionPacked Repeated ty opts = runOptM $ do
  o <- liftMB $ lookupOptionStr "packed" opts
  case o of
    (OptBool f) -> do
      case ty of
        TyPrim t | typeLabel t `elem` [LAB_VARINT,LAB_FIXED32,LAB_FIXED64]
          -> return $ if f then [OptPacked] else []
        _ -> oopsM "Only fixed width fields coudl be packed"
    _ -> oopsM "Option 'packed' must have boolean value"
optionPacked _ _ opts = runOptM $ do
  _ <- liftMB $ lookupOptionStr "packed" opts
  oopsM "Only repeated fiedls can be packed"



----------------------------------------------------------------
-- Extract option "default"
optionDefault :: Modifier -> PbType -> [Option] -> PbMonadE [PbOption]
optionDefault modif ty opts = runOptM $ do
  o <- liftMB $ lookupOptionStr "default" opts
  case modif of
    Repeated -> oopsM "repeated field cannot have default value"
    _        -> matchDefault ty o

matchDefault :: PbType -> OptionVal -> MaybeT PbMonadE [PbOption]
matchDefault (TyEnum    _)       = \_ -> oopsM "default values for enums are not suported"
matchDefault (TyMessage _)       = \_ -> oopsM "Fields with message types could not have default value"
matchDefault (TyPrim PbDouble)   = wantFloat
matchDefault (TyPrim PbFloat)    = wantFloat
matchDefault (TyPrim PbInt32)    = wantInt
matchDefault (TyPrim PbInt64)    = wantInt
matchDefault (TyPrim PbUInt32)   = wantInt
matchDefault (TyPrim PbUInt64)   = wantInt
matchDefault (TyPrim PbSInt32)   = wantInt
matchDefault (TyPrim PbSInt64)   = wantInt
matchDefault (TyPrim PbFixed32)  = wantInt
matchDefault (TyPrim PbFixed64)  = wantInt
matchDefault (TyPrim PbSFixed32) = wantInt
matchDefault (TyPrim PbSFixed64) = wantInt
matchDefault (TyPrim PbBool)     = wantBool
matchDefault (TyPrim PbString)   = wantString
matchDefault (TyPrim PbBytes)    = \_ -> oopsM "bytes field cannot have deafult value"


wantFloat :: OptionVal -> MaybeT PbMonadE [PbOption]
wantFloat o@(OptInt  _) = return [OptDefault o]
wantFloat o@(OptReal _) = return [OptDefault o]
wantFloat _             = oopsM "bad option"

wantInt :: OptionVal -> MaybeT PbMonadE [PbOption]
wantInt o@(OptInt  _) = return [OptDefault o]
wantInt _             = oopsM "bad option"

wantBool :: OptionVal -> MaybeT PbMonadE [PbOption]
wantBool o@(OptBool  _) = return [OptDefault o]
wantBool _              = oopsM "bad option"

wantString :: OptionVal -> MaybeT PbMonadE [PbOption]
wantString o@(OptString  _) = return [OptDefault o]
wantString _                = oopsM "bad option"



----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- | Convert qualified name to qname
toQName :: Qualified t (Identifier t) -> QName
toQName (Qualified path nm) = QName (map identifier path) (identifier nm)

-- | Convert field name to qname
qname :: QIdentifier -> QName
qname (FullQualId qn) = toQName qn
qname _               = error "Impossible 22"

runOptM :: MaybeT PbMonadE [a]-> PbMonadE [a]
runOptM m = fromMaybe [] <$> runMaybeT m

oopsM :: String -> MaybeT PbMonadE [a]
oopsM s = lift (oops s) >> return []

liftMB :: Monad m => Maybe a -> MaybeT m a
liftMB = MaybeT . return
