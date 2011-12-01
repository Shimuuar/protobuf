-- | 
-- Abstract syntax tree for protobuf file
module Data.Protobuf.AST where


-- | Field tag 
newtype FieldTag = FieldTag Int 

type Identifier = String 

data Modifier = Required
              | Optional
              | Repeated

-- | Primitive types
data PrimTypes = PBDouble   -- ^ Double
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

data Option  = Option Identifier String

data Message = Message_Q
                 
-- | Protocol buffers
data Protobuf = 
    Import    String
  | Package   Identifier
  | Message   Message
  | Extend    -- FIXME
  | Enum      Identifier [Option] [(Identifier,Int)]
  | TopOption Option