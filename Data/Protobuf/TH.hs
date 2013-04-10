{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Generation of instances using template haskell
module Data.Protobuf.TH (
    generateProtobuf
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Data.Int
import Data.Word
import Data.Maybe      (fromMaybe)
import Data.List       (intercalate)
import Data.ByteString (ByteString)
import Data.Sequence   (Seq)
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (qAddDependentFile)
import qualified Language.Haskell.TH.Syntax as TH
import GHC.TypeLits

import Data.Vector.HFixed (HVector(..),HVec,Fun(..),newMutableHVec,writeMutableHVec)

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

-- | Generate instances for the code
generateProtobuf :: [FilePath]  -- ^ Include path
                 -> [FilePath]  -- ^ Files to parse
                 -> Q [Dec]
generateProtobuf incs fnames = do
  -- Read protobuf files
  messages <- runIO $ loadProtobuf incs fnames
  mapM_ qAddDependentFile fnames
  -- Declare instance for every message
  case messages of
    Left  err -> fail err
    Right xs  -> concat <$> mapM genInstance xs

----------------------------------------------------------------
-- Workers
----------------------------------------------------------------

-- Generate instance for the data type
genInstance :: PbDatatype -> Q [Dec]
genInstance (PbMessage name fields) = do
  let tyFields   = map findType fields
      fieldTypes = makeTyList $ map snd tyFields
  execWriterT $ do
    -- Data instance for 'Message'
    con <- lift $ newName $ "Message_" ++ unqualifyWith '_' name
    let msgNm = return $ qstrLit name
    tellD1 $ newtypeInstD (return []) ''Message [msgNm]
              (normalC con [return (NotStrict, ConT ''HVec `AppT` fieldTypes)])
              []
    do x <- lift $ newName "x"
       tellD1 $ instanceD (return []) [t| Show (Message $(return (qstrLit name))) |]
         [ varP 'show $= lamE [conP con [varP x]]
                              [| $(TH.lift (nameBase con++" ")) ++ show $(varE x) |]
         ]
    -- HVector instance
    do v <- lift $ newName "v"
       f <- lift $ newName "f"
       tellD
         [d| instance HVector (Message $msgNm) where
               type Elems (Message $msgNm) = FieldTypes $msgNm
               construct = $( [| fmap $(conE con) construct |])
               inspect   = $(lamE [conP con [varP v], varP f]
                                  [| inspect $(varE v) $(varE f) |])
           |]
    -- Type instance for 'FieldTypes'
    tell [ TySynInstD ''FieldTypes [qstrLit name] $ fieldTypes ]
    -- Instance for 'Field' getter/setter
    forM_ (zip [0..] tyFields) $ \(i,(field,ty)) -> do
      lam <- lift $ getterTH (length tyFields) i
      tell [ InstanceD [] (ConT ''Field `AppT` qstrLit name `AppT` strLit field)
               [ TySynInstD ''FieldTy [qstrLit name, strLit field] ty
               , ValD (VarP 'getterF) (NormalB $ AppE (ConE 'Fun) lam) []
               ]
           ]
    -- Instance for 'Protobuf' (serialization/deserialization)
    deser <- lift $ deserializeDecl  name fields
    ser   <- lift $ serializtionDecl name fields
    tell [ InstanceD [] (ConT ''Protobuf `AppT` (qstrLit name))
             [ ValD (VarP 'serialize)    (NormalB $ ser  ) []
             , ValD (VarP 'getMessageST) (NormalB $ deser) []
             ]
         ]
-- Generate instances for enums
genInstance (PbEnum name fields) = execWriterT $ do
  -- Data constructor
  let constrs =
        [ NormalC (mkName con) []
        | (_,con) <- fields ]
  tell [ DataInstD [] ''Message [qstrLit name] constrs
         [''Show,''Eq,''Ord]]
  -- PbEnum instance
  let exprFrom = do
        a <- newName "a"
        lamE [varP a] $ caseE (varE a)
          [ match (conP (mkName nm) []) (normalB $ litE (integerL i)) []
          | (i,nm) <- fields ]
  let exprTo = do
        a <- newName "a"
        lamE [varP a] $ caseE (varE a) $
          [ match (litP (integerL i)) (normalB $ conE 'Just `appE`conE (mkName nm)) []
          | (i,nm) <- fields
          ]++[match wildP (normalB $ conE 'Nothing) []]
  tellD [d| instance PbEnum (Message $(return (qstrLit name))) where
              toPbEnum   = $exprTo
              fromPbEnum = $exprFrom
          |]



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
    expr = do
      names <- sequence [newName "a" | _ <- fields]
      let res = doE [noBindS $ serielizeFld nm fld | (nm,fld) <- zip names fields ]
      lamE [varP a | a <- names] res
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
deserializeDecl :: QName -> [PbField] -> Q Exp
deserializeDecl name fields = do
  updFun <- newName "updFun"
  emp    <- newName "emp"
  --
  letE [ varP emp $= [| MutableMsg $(emptyVec fields) () :: MutableMsg $(return (qstrLit name)) |]
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
       let n = [| sing :: Sing $(litT (numTyLit i)) |]
       in case modif of
          Required -> []
          Repeated -> [ [| writeMutableHVec $(varE hvec) $n $([| Seq.empty |]) |] ]
          Optional -> case [ o | OptDefault o <- opts ] of
                       []              -> [ [| writeMutableHVec $(varE hvec) $n $([| Nothing |]) |] ]
                       [OptInt    k]   -> [ [| writeMutableHVec $(varE hvec) $n $([| Just $(litE (integerL  k)) |]) |] ]
                       [OptReal   k]   -> [ [| writeMutableHVec $(varE hvec) $n $([| Just $(litE (rationalL k)) |]) |] ]
                       [OptString k]   -> [ [| writeMutableHVec $(varE hvec) $n $([| Just $(litE (stringL k))   |]) |] ]
                       [OptBool True]  -> [ [| writeMutableHVec $(varE hvec) $n $([| Just True                  |]) |] ]
                       [OptBool False] -> [ [| writeMutableHVec $(varE hvec) $n $([| Just False                 |]) |] ]
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
                      (sing :: Sing $(litT (numTyLit i)))
                      $(varE (fieldParser ty))
                      $(varE msg)
                 |]
  --
  sequence
    [ [ conP 'WireTag [intP tag, intP (getTyTag ty)]
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
        TyPrim PbString   -> ConT ''String
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
fieldParser (TyMessage _)       = 'getMessage
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

strLit :: String -> Type
strLit = LitT . StrTyLit

qstrLit :: QName -> Type
qstrLit = strLit . unqualify

unqualify :: QName -> String
unqualify = unqualifyWith '.'

unqualifyWith :: Char -> QName -> String
unqualifyWith c (QName ns n) = intercalate [c] (ns ++ [n])

makeTyList :: [Type] -> Type
makeTyList = foldr (\a ls -> PromotedConsT `AppT` a `AppT` ls) PromotedNilT

intP :: Integer -> PatQ
intP = litP . IntegerL

-- Function clause declaration for clause
($==) :: [PatQ] -> ExpQ -> ClauseQ
pats $== expr = clause pats (normalB expr) []

-- Value declaration
($=) :: PatQ -> ExpQ -> DecQ
pat $= expr = valD pat (normalB expr) []

app :: [ExpQ] -> ExpQ
app = foldl1 appE

tellD :: Q [Dec] -> WriterT [Dec] Q ()
tellD = tell <=< lift

tellD1 :: Q Dec -> WriterT [Dec] Q ()
tellD1 = tell . pure <=< lift
