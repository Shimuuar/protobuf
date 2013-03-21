{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Abstract syntax tree for protobuf file
module Data.Protobuf.AST where

import Data.List (intercalate)
import Data.Data

-- | Protocol buffer file. 
--   Parameter 'n' stands for namespace type. Since new namespaces are introduced in file 
data ProtobufFile n
  = ProtobufFile [Protobuf] [Identifier TagType] n
    deriving (Show,Typeable,Data)

-- | top level declarations
data Protobuf =
    Import      String
    -- ^ Import declaration
  | Package     (Qualified TagType (Identifier TagType))
    -- ^ Specify package for module
  | TopMessage  Message
    -- ^ Message type
  | Extend      QIdentifier [MessageField]
    -- ^ Extend message. NOT IMPLEMENTED
  | TopEnum     EnumDecl
    -- ^ Enumeration
  | TopOption   Option
    -- ^ Top level option
  deriving (Show,Typeable,Data)

-- | Enumeration declaration
data EnumDecl = EnumDecl 
                (Identifier TagType) -- Enum name
                [EnumField]          -- Enum fields
                [Identifier TagType] -- Location in namespace
                deriving (Show,Typeable,Data)

-- | Enumeration field
data EnumField
  = EnumField  (Identifier TagType) Integer
  | EnumOption Option
  deriving (Show,Typeable,Data)

-- | Message declaration
data Message = Message 
               (Identifier TagType) -- Message name
               [MessageField]       -- Message fiedls
               [Identifier TagType] -- Location in namespace (message name included)
               deriving (Show,Typeable,Data)

-- | Single field in message body. Note that groups are not supported.
data MessageField
  = MessageField Field
    -- ^ Message field
  | MessageEnum EnumDecl
    -- ^ Enumeration declaration
  | Nested Message
    -- ^ Nested message
  | MsgExtend -- FIXME
    -- ^ Extend other type
  | Extensions Extension
    -- ^ Tag range for extensions
  | MsgOption Option
    -- ^ Options for message
  deriving (Show,Typeable,Data)

-- | Tag interval for extensions
data Extension
  = Extension     Int Int
  | ExtensionOpen Int
  deriving (Show,Typeable,Data)

-- | Sinlge field of message
data Field = Field Modifier Type (Identifier TagField) FieldTag [Option]
           deriving (Show,Typeable,Data)


----------------------------------------------------------------
-- Basic types
----------------------------------------------------------------

-- | Qualified name
data Qualified t a = Qualified [Identifier t] a
                     deriving (Show,Eq,Ord,Typeable,Data)

addQualifier :: Identifier t -> Qualified t a -> Qualified t a
addQualifier q (Qualified qs x) = Qualified (q:qs) x

addQualList :: [Identifier t] -> Qualified t a -> Qualified t a
addQualList q (Qualified qs x) = Qualified (q ++ qs) x


-- | Tag for data types
data TagType   = TagType    deriving (Typeable,Data)
data TagField  = TagField   deriving (Typeable,Data)
data TagOption = TagOption  deriving (Typeable,Data)

castIdent :: Identifier t -> Identifier q
castIdent = Identifier . identifier

castQIdent :: Qualified t (Identifier t) -> Qualified q (Identifier q)
castQIdent (Qualified xs x) = Qualified (map castIdent xs) (castIdent x)

-- | Simple unqualified identifier.
newtype Identifier t = Identifier { identifier :: String }
                   deriving (Typeable,Data,Eq,Ord)
instance Show (Identifier t) where
  show = show . identifier


-- | General form of identifier
data QIdentifier 
  = QualId     [Identifier TagType] (Identifier TagType) -- ^ Qualified identifier
  | FullQualId [Identifier TagType] (Identifier TagType) -- ^ Fully qualified identifier
  deriving (Typeable,Data)

instance Show QIdentifier where
  show (QualId     ns n) = show $ intercalate "." $ map identifier (ns ++ [n])
  show (FullQualId ns n) = show $ ('.' :) $ intercalate "." $ map identifier (ns ++ [n])


-- | Field tag
newtype FieldTag = FieldTag Integer
                 deriving (Show,Typeable,Data)

-- | Modifier for data
data Modifier = Required
              | Optional
              | Repeated
              deriving (Show,Typeable,Data)
-- | Type of the field
data Type
  = SomeType QIdentifier        -- ^ Identifier of unknown type
  | MsgType  QIdentifier        -- ^ Message type
  | EnumType QIdentifier        -- ^ Enumeration type
  | BaseType PrimType           -- ^ Built-in type
  deriving (Show,Typeable,Data)

-- | Primitive types
data PrimType
  = PbDouble   -- ^ Double
  | PbFloat    -- ^ Float
  | PbInt32    -- ^ 32-bit signed integer (inefficient encoding for negative integers)
  | PbInt64    -- ^ 64-bit signed integer (inefficient encoding for negative integers)
  | PbUInt32   -- ^ 32-bit unsigned integer
  | PbUInt64   -- ^ 64-bit unsigned integer
  | PbSInt32   -- ^ 32-bit signed integer (uses zig-zag encoding)
  | PbSInt64   -- ^ 64-bit signed integer (uses zig-zag encoding)
  | PbFixed32  -- ^ Fixed size 32-bit number
  | PbFixed64  -- ^ Fixed size 64-bit number
  | PbSFixed32 -- ^ Fixed size 32-bit number
  | PbSFixed64 -- ^ Fixed size 64-bit number
  | PbBool     -- ^ Boolean
  | PbString   -- ^ UTF8 encoded string
  | PbBytes    -- ^ Byte sequence
  deriving (Show,Typeable,Data)

data Option = Option (Qualified TagOption (Identifier TagOption)) OptionVal
            deriving (Show,Typeable,Data)

lookupOption :: Qualified TagOption (Identifier TagOption) -> [Option] -> Maybe OptionVal
lookupOption _ [] = Nothing
lookupOption q (Option qi v : opts)
  | q == qi   = Just v
  | otherwise = lookupOption q opts

lookupOptionStr :: String -> [Option] -> Maybe OptionVal
lookupOptionStr = lookupOption . Qualified [] . Identifier

data OptionVal
  = OptString String
  | OptBool   Bool
  | OptInt    Integer
  | OptReal   Rational
  deriving (Show,Typeable,Data)

