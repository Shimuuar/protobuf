{
module Data.Protobuf.Grammar.Parser (
  parseProtobuf
  ) where

import Data.Protobuf.Grammar.Lexer
import Data.Protobuf.AST
import Data.Protobuf.Names

}

%name      parseProtobuf Protobuf
%tokentype { Token }
%error     { parseError }

%token
  -- Punctuation
  "{"        { TokBraceOpen  }
  "}"        { TokBraceClose }
  "["        { TokSBrkOpen   }
  "]"        { TokSBrkClose  }
  ";"        { TokSemicolon  }
  "."        { TokDot        }
  ","        { TokComma      }
  "="        { TokEqual      }
  -- Words with special meaning
  "package"  { TokIdent "package"  }  
  "import"   { TokIdent "import"   }
  "message"  { TokIdent "message"  }
  "extend"   { TokIdent "extend"   }
  "enum"     { TokIdent "enum"     }
  "option"   { TokIdent "option"   }
  "required" { TokIdent "required" }
  "optional" { TokIdent "optional" }
  "repeated" { TokIdent "repeated" }
  -- Built-in type names
  "double"   { TokIdent "double"   }
  "float"    { TokIdent "float"    }
  "int32"    { TokIdent "int32"    }
  "int64"    { TokIdent "int64"    }
  "uint32"   { TokIdent "uint32"   }
  "uint64"   { TokIdent "uint64"   }
  "sint32"   { TokIdent "sint32"   }
  "sint64"   { TokIdent "sint64"   }
  "fixed32"  { TokIdent "fixed32"  }
  "fixed64"  { TokIdent "fixed64"  }
  "sfixed32" { TokIdent "sfixed32" }
  "sfixed64" { TokIdent "sfixed64" }
  "bool"     { TokIdent "bool"     }
  "string"   { TokIdent "string"   }
  "bytes"    { TokIdent "bytes"    }
  -- Literals
  -- 
  "true"     { TokIdent  "true"  }
  "false"    { TokIdent  "false" }
  "int"      { TokInt    $$      }
  "real"     { TokDouble $$      }
  "strlit"   { TokString $$      }
  "ident"    { TokIdent  $$      }


%%

-- Complete protobuf file
Protobuf
  : {- empty -}          { [] }
  | Declaration Protobuf { $1 : $2 }
  | ";"         Protobuf { $2 }

-- Top level declaration
Declaration
  : Message { TopMessage $1 }
  | Import  { $1 }
  | Extend  { $1 }
  | Enum    { TopEnum $1 }
  | Package { $1 }
  | Option  { TopOption $1 }

Import
  : "import" "strlit" ";"     { Import $2 }
-- Message declaration
Message
  : "message" Ident "{" MessageFields "}" { Message (castIdent $2) $4 [] }
MessageFields
  : {- empty -}                { []      }
  | MessageField MessageFields { $1 : $2 }
MessageField
  : Field     { MessageField $1 }
  | Enum      { MessageEnum  $1 }
  | Message   { Nested       $1 }
    -- FIXME: extend
    -- FIXME: extension
  | Option    { MsgOption    $1 }
Field -- FIXME: field options
  : Modifier Typename Ident "=" "int" ";"                   { Field $1 $2 (castIdent $3) (FieldTag $5) [] }
  | Modifier Typename Ident "=" "int" "[" FieldOpts "]" ";" { Field $1 $2 (castIdent $3) (FieldTag $5) $7 }
FieldOpts 
  : OneOption               { [$1]    }
  | OneOption "," FieldOpts { $1 : $3 }
  
Modifier
  : "required" { Required }
  | "optional" { Optional }
  | "repeated" { Repeated }

-- Enumeration
Enum
  : "enum" Ident "{" EnumFields "}"   { EnumDecl (castIdent $2) $4 [] }
EnumFields
  : EnumField            { $1 : [] }
  | EnumField EnumFields { $1 : $2 }
EnumField
  : Option               { EnumOption $1    }
  | Ident "=" "int" ";"  { EnumField  (castIdent $1) $3 }
  
Option
  : "option" OneOption       { $2 }
OneOption  
  : QIdent "=" OptionVal { Option (castQIdent $1) $3 }
OptionVal
  : "strlit" { OptString $1    }
  | "true"   { OptBool   True  }
  | "false"  { OptBool   False }
  | "int"    { OptInt    $1    }
  | "real"   { OptReal   $1    }
Package
  : "package" QIdent ";" { Package (castQIdent $2) }
-- FIXME:
Extend
  : "extend"  { Extend  undefined undefined }


-- Identifier
Ident :: { Identifier () }
  : "ident"          { Identifier $1 }
  | "message"        { case $1 of TokIdent x -> Identifier x }
  | "bytes"          { case $1 of TokIdent x -> Identifier x }
-- Identifier which could be fully qualified
FullQualId :: { QIdentifier }
  : "." QualifiedId  { case $2 of 
                         QualId q n -> FullQualId q n 
                         _          -> error "Impossible happened: FullQualId"
                     }
  | QualifiedId      { $1 }
-- Identifier which couldn't be full qualified
QualifiedId :: { QIdentifier }
  : QIdent           { case castQIdent $1 of 
                         Qualified xs x -> QualId xs x
                     }
-- Worker for qulified identifiers
QIdent :: { Qualified () (Identifier ()) }
  : Ident            { Qualified [] $1 }
  | Ident "." QIdent { case $3 of
                         Qualified qs x -> Qualified ($1 : qs) x 
                     }

-- Type declarations
Typename
  : BuiltinType { BaseType $1 }
  | FullQualId  { SomeType $1 }
BuiltinType
  : "double"    { PbDouble   }
  | "float"     { PbFloat    }
  | "int32"     { PbInt32    }
  | "int64"     { PbInt64    }
  | "uint32"    { PbUInt32   }
  | "uint64"    { PbUInt64   }
  | "sint32"    { PbSInt32   }
  | "sint64"    { PbSInt64   }
  | "fixed32"   { PbFixed32  }
  | "fixed64"   { PbFixed64  }
  | "sfixed32"  { PbSFixed32 }
  | "sfixed64"  { PbSFixed64 }
  | "bool"      { PbBool     }
  | "string"    { PbString   }
  | "bytes"     { PbBytes    }
  
{

parseError :: [Token] -> a
parseError = error . ("ERROR: " ++) . show

castIdent :: Identifier t -> Identifier q
castIdent = Identifier . identifier

castQIdent :: Qualified t (Identifier t) -> Qualified q (Identifier q)
castQIdent (Qualified xs x) = Qualified (map castIdent xs) (castIdent x)

}
