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

import Data.Functor ((<$>))
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
    cnvMessage (Message nm fields)
      =  PbMessage (makeQN nm)
     <$> (concat <$> mapM cnvField fields)
    -- Convert and check field
    cnvField (MessageField (Field modif ty name (FieldTag tag) opts)) = do
      optPacked <-
        case (modif,lookupOptionStr "packed" opts) of
          -- No option
          (_,Nothing) -> return []
          -- Correct options
          (Repeated, Just (OptBool False)) -> return []
          (Repeated, Just (OptBool True )) -> return [OptPacked]
          -- Handle incorrect cases
          _ -> oops "Incorrect packed option" >> return []
      optDefault <-
        case (modif, lookupOptionStr "default" opts) of
          (_,        Nothing) -> return []
          (Repeated, Just _ ) -> oops "repeated field cannot have default value" >> return []
          (_       , Just v ) -> matchDefault fType v
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
    cnvEnum (EnumDecl nm fields)
      = PbEnum (makeQN nm)
        [ (i,name) | EnumField (Identifier name) i <- fields]
    -- names
    makeQN (Qualified path nm) = QName (map identifier path) (identifier nm)
    --
    qname (FullQualId qn) = makeQN qn
    qname _               = error "Impossible 22"


matchDefault :: PbType -> OptionVal -> PbMonadE [PbOption]
matchDefault (TyEnum    _)       = \_ -> oops "default values for enums are not suported" >> return []
matchDefault (TyMessage _)       = \_ -> oops "Fields with message types could not have default value" >> return []
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
matchDefault (TyPrim PbBytes)    = \_ -> oops "bytes field cannot have deafult value" >> return []


wantFloat :: OptionVal -> PbMonadE [PbOption]
wantFloat o@(OptInt  _) = return [OptDefault o]
wantFloat o@(OptReal _) = return [OptDefault o]
wantFloat _             = oops "bad option" >> return []

wantInt :: OptionVal -> PbMonadE [PbOption]
wantInt o@(OptInt  _) = return [OptDefault o]
wantInt _             = oops "bad option" >> return []

wantBool :: OptionVal -> PbMonadE [PbOption]
wantBool o@(OptBool  _) = return [OptDefault o]
wantBool _              = oops "bad option" >> return []

wantString :: OptionVal -> PbMonadE [PbOption]
wantString o@(OptString  _) = return [OptDefault o]
wantString _                = oops "bad option" >> return []
