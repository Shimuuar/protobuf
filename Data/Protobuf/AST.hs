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

-- | Enumeration declaration
data EnumDecl = EnumDecl Identifier [EnumField]

-- | Enumeration field
data EnumField 
  = EnumField  Identifier Integer
  | EnumOption Option

-- | Message declaration 
data Message = Message Identifier [MessageField]

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

-- | Tag interval for extensions
data Extension
  = Extension     Int Int
  | ExtensionOpen Int

-- | Sinlge field of message
data Field = 
  Field Modifier Type Identifier FieldTag [Option]


          
----------------------------------------------------------------
-- Basic types
----------------------------------------------------------------

-- | Identifier
newtype Identifier = Identifier String

-- | Qualified identifier
data QIdentifier = QIdentifier [Identifier] Identifier



-- | Field tag 
newtype FieldTag = FieldTag Integer


-- | Modifier for data
data Modifier = Required
              | Optional
              | Repeated

-- | Type of the field
data Type 
  = UserType Identifier
  | BaseType PrimType

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

data Option  = Option QIdentifier String
