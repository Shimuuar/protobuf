{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Abstract syntax tree for protobuf file
--
-- Message and enum names are represented as their name and path to
-- the name in global namespace.
module Data.Protobuf.Internal.AST (
    -- * Protobuf AST
    Protobuf(..)
    -- ** Enums
  , EnumDecl(..)
  , EnumField(..)
    -- ** Messages
  , Message(..)
  , MessageField(..)
  , Extension(..)
  , Field(..)
    -- * Basic types
  , QIdentifier(..)
  , FieldTag(..)
    -- * Protobuf types
  , Modifier(..)
  , Type(..)
  , PrimType(..)
  , TypeLabel(..)
  , typeLabel
    -- * Options
  , Option(..)
  , OptionVal(..)
  , lookupOption
  , lookupOptionStr
  ) where

import Data.Data
import Data.Protobuf.Internal.Names

-- | Top level declarations
data Protobuf =
    Import      String
    -- ^ Import declaration
  | Package     (QualifiedId TagType)
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

-- | Enumeration declaration. Parameters are
--
--   * Enum name
--
--   * Fields of enumeration
data EnumDecl = EnumDecl 
                (QualifiedId TagType)
                [EnumField]
                deriving (Show,Typeable,Data)

-- | Enumeration field
data EnumField
  = EnumField  (Identifier TagType) Integer
  | EnumOption Option
  deriving (Show,Typeable,Data)

-- | Message declaration
--
--   * Message name
--
--   * Message fields
data Message = Message 
               (QualifiedId TagType)
               [MessageField]
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

-- | Data type identifier
data QIdentifier 
  = QualId     (QualifiedId TagType) -- ^ Qualified identifier
  | FullQualId (QualifiedId TagType) -- ^ Fully qualified identifier
  deriving (Typeable,Data)

instance Show QIdentifier where
  show (QualId     n) = show n
  show (FullQualId n) = '.' : show n


-- | Field tag
newtype FieldTag = FieldTag Integer
                 deriving (Show,Typeable,Data)

-- | Modifier for data
data Modifier = Required
              | Optional
              | Repeated
              deriving (Show,Eq,Typeable,Data)

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
  deriving (Show,Eq,Typeable,Data)

-- | Type label which is used in encoding
data TypeLabel
  = LAB_VARINT
  | LAB_FIXED64
  | LAB_LENDELIM
  | LAB_STARTGR
  | LAB_ENDGR
  | LAB_FIXED32
  deriving (Show,Eq,Enum,Typeable,Data)

typeLabel :: PrimType -> TypeLabel
typeLabel PbDouble   = LAB_FIXED64
typeLabel PbFloat    = LAB_FIXED32
typeLabel PbInt32    = LAB_VARINT
typeLabel PbInt64    = LAB_VARINT
typeLabel PbUInt32   = LAB_VARINT
typeLabel PbUInt64   = LAB_VARINT
typeLabel PbSInt32   = LAB_VARINT
typeLabel PbSInt64   = LAB_VARINT
typeLabel PbFixed32  = LAB_FIXED32
typeLabel PbFixed64  = LAB_FIXED64
typeLabel PbSFixed32 = LAB_FIXED32
typeLabel PbSFixed64 = LAB_FIXED64
typeLabel PbBool     = LAB_VARINT
typeLabel PbString   = LAB_LENDELIM
typeLabel PbBytes    = LAB_LENDELIM


data Option = Option (QualifiedId TagOption) OptionVal
            deriving (Show,Typeable,Data)

lookupOption :: QualifiedId TagOption -> [Option] -> Maybe OptionVal
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
  deriving (Show,Eq,Typeable,Data)
