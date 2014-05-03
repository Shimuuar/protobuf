{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Generation of instances using template haskell
module Data.Protobuf.TH (
    -- * Code generation
    generateProtobuf
  , PbGenOption(..)
    -- * Lens
  , elm
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Data.Int
import Data.Word
import Data.Text       (Text)
import Data.List       (intercalate)
import Data.ByteString (ByteString)
import Data.Sequence   (Seq)
import Data.Typeable   (Typeable)
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (qAddDependentFile)
import Language.Haskell.TH.Quote
import qualified Language.Haskell.TH.Syntax as TH
import GHC.TypeLits

import Data.Vector.Fixed (S,Z)
import qualified Data.Vector.HFixed as H
import Data.Vector.HFixed      (HVector(..),element)
import Data.Vector.HFixed.Class(Fun(..))
import Data.Vector.HFixed.HVec (HVec,newMutableHVec,writeMutableHVec)

import Data.Protobuf
import Data.Protobuf.API
import Data.Protobuf.Internal.AST hiding (Type,Message,Field,Protobuf)
import Data.Protobuf.Serialize.Protobuf
import Data.Protobuf.Serialize.VarInt
import Data.Serialize.IEEE754
import Data.Serialize.Get
import Data.Serialize.Put
import Data.Serialize


----------------------------------------------------------------
-- API
----------------------------------------------------------------

-- | Options for data generation
data PbGenOption
  = MsgRecord String
  -- ^ Generate instance for Message using ADT not a newtype wrapped
  -- 'HVec'.


-- | Generate instances for the code
generateProtobuf :: [PbGenOption]  -- ^ Options for code generation.
                 -> [FilePath]     -- ^ Include path
                 -> [FilePath]     -- ^ Files to parse
                 -> Q [Dec]
generateProtobuf opts incs fnames = do
  -- Read protobuf files
  messages <- runIO $ loadProtobuf incs fnames
  mapM_ qAddDependentFile fnames
  -- Declare instance for every message
  case messages of
    Left  err -> fail err
    Right xs  -> concat <$> mapM (genInstance opts) xs

-- | Sugar for the field access
elm :: QuasiQuoter
elm = QuasiQuoter
  { quoteExp  = \s -> (varE 'field) `appE` [| sing :: Sing $(litT (strTyLit s)) |]
  , quotePat  = error "No pattern quasi quotation"
  , quoteType = error "No type quasi quotation"
  , quoteDec  = error "No declaration quasi quotation"
  }


----------------------------------------------------------------
-- Workers
----------------------------------------------------------------

-- Type of message to generate
data MessageType
  = MessageHVec        -- Use HVec
  | MessageRec         -- Generate normal haskell record
  | MessageExists Type -- instance exists already do nothing

-- Generate instance for the data type
genInstance :: [PbGenOption] -> PbDatatype -> Q [Dec]
genInstance opts (PbMessage name fields) = do
  let tyFields   = map findType fields
      fieldTypes = makeTyList $ map snd tyFields
      qualName   = unqualify name
      msgNm      = return $ qstrLit name
  execWriterT $ do
    -- First we want to check whether data instance is already
    -- defined by user. If so data type and Show/HVector instances
    -- will not be generated
    toGen <- lift $ chooseGenerator qualName opts
    ty <- case toGen of
            MessageHVec      -> genMessageHVec msgNm name fieldTypes
            MessageRec       -> genMessageRec  msgNm name tyFields fieldTypes
            MessageExists ty -> return ty
    -- Type instance for 'FieldTypes'
    tellD1 $
      tySynInstD ''FieldTypes [msgNm] (return fieldTypes)
    -- Generate Eq/Show instances
    case toGen of
      MessageExists _ -> return ()
      _               -> do
        tellD1 $
          instanceD (return []) (conT ''Eq `appT` return ty)
            [ varP '(==) $= [| H.eq |] ]
        tellD1 $
          instanceD (return []) (conT ''Show `appT` return ty)
            [ varP 'show $= [| \v -> "MSG: " ++ intercalate ", "
                                  (H.foldr (H.Proxy :: H.Proxy Show) (\x xs -> show x : xs) [] v)
                            |]
            ]

    -- Instance for 'Field' getter/setter
    forM_ (zip [0..] tyFields) $ \(i,(fld,ty)) -> do
      -- NOTE: splices of data declarations couldn't be used there
      --       because GHC will complaint that type variables in
      --       instance head and in the type synonym are different.
      --       See bug #4230 for discussion
      tellD1 $ do
        instanceD (return []) (conT ''Field `appT` msgNm `appT` strLit fld)
          [ tySynInstD ''FieldTy [msgNm, strLit fld] (return ty)
          , varP 'fieldLens $= [| \_ _ -> $(varE 'element `appE` singNat i) |]
          ]
    -- Instance for 'Protobuf' (serialization/deserialization)
    --
    -- NOTE: same caveat as above about splices apply
    deser <- lift $ deserializeDecl  name fields ty
    ser   <- lift $ serializtionDecl name fields
    tell [ InstanceD [] (ConT ''Protobuf `AppT` ty)
             [ TySynInstD ''MessageName [ty] (qstrLit name) 
             , ValD (VarP 'serialize)    (NormalB $ ser  ) []
             , ValD (VarP 'getMessageST) (NormalB $ deser) []
             ]
         ]
    return ()
-- Generate instances for enums
genInstance _ (PbEnum name fields) = execWriterT $ do
  let msgNm = return $ qstrLit name
  let con   = mkName $ unqualifyWith '_' name
  -- Data constructor
  tellD1 $
    dataD (return []) con []
      [ normalC (mkName c) [] | (_,c) <- fields ]
      [''Show,''Eq,''Ord]
  tellD1 $
    tySynInstD ''MessageName [msgNm] (conT con)
  -- PbEnum instance
  let exprFrom = do
        a <- newName "a"
        lamE [varP a] $ caseE (varE a)
          [ conP (mkName nm) [] --> TH.lift i | (i,nm) <- fields ]
  let exprTo = do
        a <- newName "a"
        lamE [varP a] $ caseE (varE a) $
          [ litP (integerL i) --> [| Just $(conE (mkName nm)) |] | (i,nm) <- fields ]++
          [ wildP             --> [| Nothing |] ]
  tellD [d| instance PbEnum (conT con) where
              toPbEnum   = $exprTo
              fromPbEnum = $exprFrom
          |]


-- Choose generator for protobuf message.
chooseGenerator :: String        -- Qualified message name
                -> [PbGenOption] -- Options for TH generator
                -> Q MessageType
chooseGenerator qualName opts = do
  -- Check that type instance for Message already defined
  FamilyI _ instances <- reify ''Message
  let defined = [ty | TySynInstD _ [LitT (StrTyLit s)] ty <- instances
                    , s == qualName
                ]
  -- Check whether we need to generate data as haskell record
  let useRecs = not $ null [() | MsgRecord nm <- opts, nm == qualName]
  case defined of
    [ty]          -> return $ MessageExists ty
    _ | useRecs   -> return MessageRec
      | otherwise -> return MessageHVec

-- Generate message as newtype wrapper over HVec
genMessageHVec :: TypeQ         -- Name of message as type (FIXME: duplication)
               -> QName         -- Name of message
               -> Type          -- Type level list of message elements
               -> WriterT [Dec] Q Type
genMessageHVec msgNm name fieldTypes = do
  let con = mkName $ unqualifyWith '_' name
  tellD1 $ newtypeD (return []) con []
             (normalC con [return (NotStrict, ConT ''HVec `AppT` fieldTypes)]) [''Typeable]
  tellD1 $ tySynInstD ''Message [msgNm] (conT con)
  -- HVector instance for message
  v <- lift $ newName "v"
  f <- lift $ newName "f"
  -- Cannot use splices (GHC bug #4230)
  tellD1 $
    instanceD (return []) (conT ''HVector `appT` conT con)
      [ tySynInstD ''Elems [conT con] (return fieldTypes)
      , varP 'construct $= [| fmap $(conE con) construct |]
      , varP 'inspect   $= lamE [conP con [varP v], varP f]
                             [| inspect $(varE v) $(varE f) |]
      , pragInlD 'construct Inline FunLike AllPhases
      , pragInlD 'inspect   Inline FunLike AllPhases
      ]
  lift $ conT con

-- Generate instance for `Message' using haskell records
genMessageRec :: TypeQ            -- Name of message as type (FIXME: duplication)
              -> QName            -- Name of message
              -> [(String, Type)] -- List of pairs (field name, field type)
              -> Type          -- Type level list of message elements
              -> WriterT [Dec] Q Type
genMessageRec msgNm name tyFields fieldTypes = do
  -- Generate data type and derived Show instance
  let conTy = mkName $ unqualifyWith '_' name
      con   = mkName $ case name of QName _ s -> s
  let flds = [return $ (mkName nm,IsStrict,ty) | (nm,ty) <- tyFields]
  tellD1 $ dataD (return []) conTy [] [recC con flds] [''Typeable]
  tellD1 $ tySynInstD ''Message [msgNm] (conT conTy)
  -- Derive HVector instance
  xs <- lift $ mapM newName ["x" | _ <- tyFields]
  f  <- lift $ newName "f"
  -- Cannot use splices (GHC bug #4230)
  tellD1 $
    instanceD (return []) (conT ''HVector `appT` conT conTy)
      [ tySynInstD ''Elems [conT conTy] (return fieldTypes)
      , varP 'construct $= (conE 'Fun `appE` conE con)
      , varP 'inspect   $= ( lamE [ conP con  (map varP xs)
                                  , conP 'Fun [varP f]
                                  ]
                           $ foldl appE (varE f) (map varE xs)
                           )
      , pragInlD 'construct Inline FunLike AllPhases
      , pragInlD 'inspect   Inline FunLike AllPhases
      ]
  lift $ conT conTy

-- Getter function
getterTH :: Int -> Int -> Q Exp
getterTH nTot n = do
  nm <- newName "x"
  lamE
    [ if i == n then varP nm else wildP | i <- take nTot [0..]]
    (varE nm)

-- Declaration of serialization function.
serializtionDecl :: QName -> [PbField] -> Q Exp
serializtionDecl nm fields
  = [| flip inspect (Fun $expr) |]
  where
    -- Function body for serialization
    expr = do
      names <- sequence [newName "a" | _ <- fields]
      let res = doE [noBindS $ serielizeFld n fld | (n,fld) <- zip names fields ]
      lamE [varP a | a <- names] res
    -- Serialize individual fields
    serielizeFld a (PbField Required ty _ tag _) =
      [| put (WireTag $(TH.lift tag) $(TH.lift (getTyTag ty))) >> $(varE (fieldWriter ty)) $(varE a) |]
    serielizeFld a (PbField Optional ty _ tag _) =
      [| putOptional (\x -> put (WireTag $(TH.lift tag) $(TH.lift (getTyTag ty))) >> $(varE (fieldWriter ty)) x) $(varE a) |]
    serielizeFld a (PbField Repeated ty _ tag opts) =
      case [() | OptPacked <- opts] of
        []  -> [| F.forM_ $(varE a) $ \x -> put (WireTag $(TH.lift tag) $(TH.lift (getTyTag ty))) >> $(varE (fieldWriter ty)) x |]
        [_] -> [| put (WireTag $(TH.lift tag) 2) >> putPacked $(varE (fieldWriter ty)) $(varE a) |]
        _   -> error "Internal error"

-- Declaration of deserialization function. General layout
--
-- > getRecords updFun emptyRec
deserializeDecl :: QName -> [PbField] -> Type -> Q Exp
deserializeDecl name fields ty = do
  updFun <- newName "updFun"
  emp    <- newName "emp"
  --
  letE [ varP emp $= [| MutableMsg $(emptyVec fields) () :: MutableMsg $(return ty) |]
       , updateDecl updFun (zip [0..] fields)
       ]
       [| getRecords $(varE updFun) $(varE emp) |]

-- Uninitialized vector
emptyVec :: [PbField] -> Q Exp
emptyVec fields = do
  hvec <- newName "hvec"
  doE $ concat
    [ [ bindS (varP hvec) [| newMutableHVec |] ]
    , map noBindS $ flip concatMap (zip [0..] fields) $ \(i, PbField modif name ty _ opts) ->
       let n = singNat i
       in case modif of
          Required -> []
          Repeated -> [ [| $(varE 'writeMutableHVec) $(varE hvec) $n $([|Seq.empty|]) |] ]
          Optional -> case [ o | OptDefault o <- opts ] of
                       []              -> [ [| $(varE 'writeMutableHVec) $(varE hvec) $n $([| Nothing |]) |] ]
                       [OptInt    k]   -> [ [| $(varE 'writeMutableHVec) $(varE hvec) $n $([| Just $(litE (integerL  k)) |]) |] ]
                       [OptReal   k]   -> [ [| $(varE 'writeMutableHVec) $(varE hvec) $n $([| Just $(litE (rationalL k)) |]) |] ]
                       [OptString k]   -> [ [| $(varE 'writeMutableHVec) $(varE hvec) $n $([| Just $(litE (stringL k))   |]) |] ]
                       [OptBool True]  -> [ [| $(varE 'writeMutableHVec) $(varE hvec) $n $([| Just True                  |]) |] ]
                       [OptBool False] -> [ [| $(varE 'writeMutableHVec) $(varE hvec) $n $([| Just False                 |]) |] ]
                       _ -> error "Ay-ay-ay"
        -- where
    , [noBindS [| return $(varE hvec) |]]
    ]


-- Function for updating single record
updateDecl :: Name -> [(Integer, PbField)] -> Q Dec
updateDecl funNm fields = do
  -- Primary clauses
  cls <- mapM updateClause fields
  -- Fallback clause
  fallback <- do
    wt  <- newName "wt"
    msg <- newName "msg"
    [varP wt, varP msg]
      $== [| skipUnknownField $(varE wt) >> return $(varE msg) |]
  --
  return $ FunD funNm (concat cls ++ [fallback])

-- Update clause for single field
updateClause :: (Integer, PbField) -> Q [Clause]
updateClause (i,(PbField modif ty _ tag opts)) = do
  msg <- newName "msg"
  -- Generate
  let updater =
        case modif of
          Required -> varE 'writeRequired
          Optional -> varE 'writeOptional
          Repeated -> case [ () | OptPacked <- opts ] of
            []  -> varE 'writeRepeated
            [_] -> varE 'writeRepeatedPacked
            _   -> error "Internal error"
      updExpr = [| $updater
                      $(singNat i)
                      $(varE (fieldParser ty))
                      $(varE msg)
                 |]
  --
  let wireTy = case [ () | OptPacked <- opts ] of
                 []  -> getTyTag ty
                 [_] -> 2
                 _   -> error "Internal error"
  sequence
    [ [ conP 'WireTag [intP tag, intP wireTy]
      , varP msg
      ] $== updExpr
    , [ conP 'WireTag [intP tag, wildP]
      , wildP
      ] $== [| fail "Bad wire tag" |]
    ]

----------------------------------------------------------------
-- TH helpers
----------------------------------------------------------------

-- Produce pair (name, type) for field of the message.
findType :: PbField -> (String, Type)
findType (PbField m ty name _ _)
  = (name, modifyTy baseTy)
  where
    modifyTy =
      case  m of
        Required -> id
        Optional -> AppT $ ConT ''Maybe
        Repeated -> AppT $ ConT ''Seq
    -- Basic type where (required/optional/repeated) modifiers are not
    -- applied
    baseTy =
      case ty of
        -- Primitive types
        TyPrim PbDouble   -> ConT ''Double
        TyPrim PbFloat    -> ConT ''Float
        TyPrim PbInt32    -> ConT ''Int32
        TyPrim PbInt64    -> ConT ''Int64
        TyPrim PbUInt32   -> ConT ''Word32
        TyPrim PbUInt64   -> ConT ''Word64
        TyPrim PbSInt32   -> ConT ''Int32
        TyPrim PbSInt64   -> ConT ''Int64
        TyPrim PbFixed32  -> ConT ''Int32
        TyPrim PbFixed64  -> ConT ''Int64
        TyPrim PbSFixed32 -> ConT ''Int32
        TyPrim PbSFixed64 -> ConT ''Int64
        TyPrim PbBool     -> ConT ''Bool
        TyPrim PbString   -> ConT ''Text
        TyPrim PbBytes    -> ConT ''ByteString
        -- Custom types
        TyMessage nm      -> ConT ''Message `AppT` qstrLit nm
        TyEnum    nm      -> ConT ''Message `AppT` qstrLit nm


-- Get type tag which corresponds to the given type
getTyTag :: PbType -> Integer
getTyTag (TyPrim    ty) = fromIntegral $ fromEnum $ typeLabel ty
getTyTag (TyMessage _ ) = fromIntegral $ fromEnum LAB_LENDELIM
getTyTag (TyEnum    _ ) = fromIntegral $ fromEnum LAB_VARINT


-- Name parser function for the given type
fieldParser :: PbType -> Name
fieldParser (TyPrim PbDouble)   = 'getFloat64le
fieldParser (TyPrim PbFloat)    = 'getFloat32le
fieldParser (TyPrim PbInt32)    = 'getVarInt32
fieldParser (TyPrim PbInt64)    = 'getVarInt64
fieldParser (TyPrim PbUInt32)   = 'getVarWord32
fieldParser (TyPrim PbUInt64)   = 'getVarWord64
fieldParser (TyPrim PbSInt32)   = 'getZigzag32
fieldParser (TyPrim PbSInt64)   = 'getZigzag64
fieldParser (TyPrim PbFixed32)  = 'getWord32le
fieldParser (TyPrim PbFixed64)  = 'getWord64le
fieldParser (TyPrim PbSFixed32) = 'getInt32le
fieldParser (TyPrim PbSFixed64) = 'getInt64le
fieldParser (TyPrim PbBool)     = 'getVarBool
fieldParser (TyPrim PbString)   = 'getPbString
fieldParser (TyPrim PbBytes)    = 'getPbBytestring
-- Custom types
fieldParser (TyMessage _)       = 'getDelimited
fieldParser (TyEnum    _)       = 'getPbEnum


-- Name parser function for the given type
fieldWriter :: PbType -> Name
fieldWriter (TyPrim PbDouble)   = 'putFloat64le
fieldWriter (TyPrim PbFloat)    = 'putFloat32le
fieldWriter (TyPrim PbInt32)    = 'putVarInt32
fieldWriter (TyPrim PbInt64)    = 'putVarInt64
fieldWriter (TyPrim PbUInt32)   = 'putVarWord32
fieldWriter (TyPrim PbUInt64)   = 'putVarWord64
fieldWriter (TyPrim PbSInt32)   = 'putZigzag32
fieldWriter (TyPrim PbSInt64)   = 'putZigzag64
fieldWriter (TyPrim PbFixed32)  = 'putWord32le
fieldWriter (TyPrim PbFixed64)  = 'putWord64le
fieldWriter (TyPrim PbSFixed32) = 'putInt32le
fieldWriter (TyPrim PbSFixed64) = 'putInt64le
fieldWriter (TyPrim PbBool)     = 'putVarBool
fieldWriter (TyPrim PbString)   = 'putPbString
fieldWriter (TyPrim PbBytes)    = 'putPbBytestring
-- Custom types
fieldWriter (TyMessage _)       = 'serialize
fieldWriter (TyEnum    _)       = 'putPbEnum

----------------------------------------------------------------
-- TH helpers
----------------------------------------------------------------

singNat :: Integer -> ExpQ
singNat i = [|undefined :: $(foldr appT (conT ''Z) (replicate (fromIntegral i) (conT ''S))) |]

strLit :: String -> Q Type
strLit = return . LitT . StrTyLit

qstrLit :: QName -> Type
qstrLit = LitT . StrTyLit . unqualify

unqualify :: QName -> String
unqualify = unqualifyWith '.'

unqualifyWith :: Char -> QName -> String
unqualifyWith c (QName ns n) = intercalate [c] (ns ++ [n])

makeTyList :: [Type] -> Type
makeTyList = foldr (\a ls -> PromotedConsT `AppT` a `AppT` ls) PromotedNilT

intP :: Integer -> PatQ
intP = litP . IntegerL


-- Shorhand for match inside case expression
(-->) :: PatQ -> ExpQ -> MatchQ
pat --> expr = match pat (normalB expr) []

-- Function clause declaration for clause
($==) :: [PatQ] -> ExpQ -> ClauseQ
pats $== expr = clause pats (normalB expr) []

-- Value declaration
($=) :: PatQ -> ExpQ -> DecQ
pat $= expr = valD pat (normalB expr) []



tellD :: Q [Dec] -> WriterT [Dec] Q ()
tellD = tell <=< lift

tellD1 :: Q Dec -> WriterT [Dec] Q ()
tellD1 = tell . pure <=< lift
