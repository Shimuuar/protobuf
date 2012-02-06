-- |
-- Abstract syntax tree for protobuf file
module Data.Protobuf.AST where

-- | top level declarations
data Protobuf =
    Import      String
    -- ^ Import declaration
  | Package     QIdentifier
    -- ^ Specify package for module
  | MessageDecl Message
    -- ^ Message type
  | Extend      QIdentifier [MessageField]
    -- ^ Extend message. NOT IMPLEMENTED
  | TopEnum     EnumDecl
    -- ^ Enumeration
  | TopOption   Option
    -- ^ Top level option
  deriving Show

-- | Enumeration declaration
data EnumDecl = EnumDecl Identifier [EnumField]
                deriving Show

-- | Enumeration field
data EnumField
  = EnumField  Identifier Integer
  | EnumOption Option
  deriving Show

-- | Message declaration
data Message = Message Identifier [MessageField]
             deriving Show

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
  deriving Show

-- | Tag interval for extensions
data Extension
  = Extension     Int Int
  | ExtensionOpen Int
  deriving Show

-- | Sinlge field of message
data Field = Field Modifier Type Identifier FieldTag [Option]
           deriving Show


----------------------------------------------------------------
-- Basic types
----------------------------------------------------------------

-- | Simple unqualified identifier.
newtype Identifier = Identifier String
                   deriving Show

-- | General form of identifier
data QIdentifier 
  = QualId     [Identifier] Identifier -- ^ Qualified identifier
  | FullQualId [Identifier] Identifier -- ^ Fully qualified identifier
  deriving Show


-- | Field tag
newtype FieldTag = FieldTag Integer
                 deriving Show

-- | Modifier for data
data Modifier = Required
              | Optional
              | Repeated
              deriving Show
-- | Type of the field
data Type
  = UserType QIdentifier
  | BaseType PrimType
  deriving Show

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
  deriving Show

data Option  = Option QIdentifier String
             deriving Show
