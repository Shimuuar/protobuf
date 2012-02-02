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
  | Extend    -- FIXME
    -- ^ Extend message. NOT IMPLEMENTED
  | TopEnum     EnumDecl
    -- ^ Enumeration
  | TopOption   Option
    -- ^ Top level option

-- | Enumeration declaration
data EnumDecl = EnumDecl Identifier [Option] [(Identifier,Int)]

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
newtype FieldTag = FieldTag Int 


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
  = PBDouble   -- ^ Double
  | PBFloat    -- ^ Float
  | PBInt32    -- ^ 32-bit signed integer (inefficient encoding for negative integers)
  | PBInt64    -- ^ 64-bit signed integer (inefficient encoding for negative integers)
  | PBUInt32   -- ^ 32-bit unsigned integer
  | PBUInt64   -- ^ 64-bit unsigned integer
  | PBSInt32   -- ^ 32-bit signed integer (uses zig-zag encoding)
  | PBSInt64   -- ^ 64-bit signed integer (uses zig-zag encoding)
  | PBFixed32  -- ^ Fixed size 32-bit number
  | PBFixed64  -- ^ Fixed size 64-bit number
  | PBSFixed32 -- ^ Fixed size 32-bit number
  | PBSFixed64 -- ^ Fixed size 64-bit number
  | PBBool     -- ^ Boolean
  | PBString   -- ^ UTF8 encoded string
  | PBBytes    -- ^ Byte sequence

data Option  = Option QIdentifier String
