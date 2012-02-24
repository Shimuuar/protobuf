{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Abstract syntax tree for protobuf file
module Data.Protobuf.AST where

import Data.Data

-- | Protocol buffer file. 
--   Parameter 'n' stands for namespace type. Since new namespaces are introduced in file 
data ProtobufFile n
  = ProtobufFile [Protobuf n] [Identifier] n (Global n)

-- | Newtype wrapper which is used to ditinguish between package
--   namespace and global namespace
newtype Global a = Global a

-- | top level declarations
data Protobuf n =
    Import      String
    -- ^ Import declaration
  | Package     [Identifier]
    -- ^ Specify package for module
  | TopMessage  (Message n)
    -- ^ Message type
  | Extend      QIdentifier [MessageField n]
    -- ^ Extend message. NOT IMPLEMENTED
  | TopEnum     EnumDecl
    -- ^ Enumeration
  | TopOption   Option
    -- ^ Top level option
  deriving (Show,Typeable,Data)

-- | Enumeration declaration
data EnumDecl = EnumDecl Identifier [EnumField]
                deriving (Show,Typeable,Data)

-- | Enumeration field
data EnumField
  = EnumField  Identifier Integer
  | EnumOption Option
  deriving (Show,Typeable,Data)

-- | Message declaration
data Message n = Message Identifier [MessageField n] n
                 deriving (Show,Typeable,Data)

-- | Single field in message body. Note that groups are not supported.
data MessageField n
  = MessageField Field
    -- ^ Message field
  | MessageEnum EnumDecl
    -- ^ Enumeration declaration
  | Nested (Message n)
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
data Field = Field Modifier Type Identifier FieldTag [Option]
           deriving (Show,Typeable,Data)


----------------------------------------------------------------
-- Basic types
----------------------------------------------------------------

-- | Qualified name
data Qualified a = Qualified [Identifier] a
                   deriving (Show,Eq,Ord)

addQualifier :: Identifier -> Qualified a -> Qualified a
addQualifier q (Qualified qs x) = Qualified (q:qs) x

addQualList :: [Identifier] -> Qualified a -> Qualified a
addQualList q (Qualified qs x) = Qualified (q ++ qs) x


-- | Simple unqualified identifier.
newtype Identifier = Identifier { identifier :: String }
                   deriving (Typeable,Data,Eq,Ord)
instance Show Identifier where
  show = show . identifier

-- | General form of identifier
data QIdentifier 
  = QualId     [Identifier] Identifier -- ^ Qualified identifier
  | FullQualId [Identifier] Identifier -- ^ Fully qualified identifier
  deriving (Show,Typeable,Data)


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
  = UserType QIdentifier
  | BaseType PrimType
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

data Option  = Option QIdentifier String
             deriving (Show,Typeable,Data)
