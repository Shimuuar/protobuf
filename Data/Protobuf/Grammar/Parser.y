{
module Data.Protobuf.Grammar.Parser where

import Data.Protobuf.Grammar.Lexer
import Data.Protobuf.AST
}

%name      parseProtobuf
%tokentype { Token }
%error     { parseError }

%token
  -- Literals
  "int"      { TokInt    $$  }
  "real"     { TokDouble $$  }
  "strlit"   { TokString $$  }
  "ident"    { TokIdent  $$  }
  -- Punctuation
  "{"        { TokBraceOpen  }
  "}"        { TokBraceClose }
  ";"        { TokSemicolon  }
  "."        { TokDot        }
  "="        { TokEqual      }
  -- Reserved words
  "message"  { TokIdent "message"  }
  "import"   { TokIdent "import"   }
  "option"   { TokIdent "option"   }
  "enum"     { TokIdent "enum"     }
  "package"  { TokIdent "package"  }
  "extend"   { TokIdent "extend"   }
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
%%

-- Complete protobuf file
Protobuf :: { [Protobuf] }
Protobuf 
  : {- empty -}          { [] }
  | Declaration Protobuf { $1 : $2 }
  | ";"         Protobuf { $2 }

-- Top level declaration
Declaration :: { Protobuf }
Declaration
  : Message { $1 }
  | Import  { $1 }
  | Extend  { $1 }
  | Enum    { $1 }
  | Package { $1 }
  | Option  { $1 }

Import
  : "import" "strlit" ";"     { Import $2 }
Message
  : "message" Ident "{" "}" { MessageDecl (Message $2 []) }
-- FIXME: option value could be almost anything!
Option
  : "option"  QIdent "=" "strlit" { TopOption (Option $2 $4) }
Package
  : "package" QIdent ";" { Package $2 }
-- FIXME:
Extend
  : "extend"  { Extend }
Enum
  : "enum"    { TopEnum undefined }



-- Identifiers
Ident
  : "ident"          { Identifier $1 }
QIdent 
  : Ident            { QIdentifier [] $1 }
  | QIdent "." Ident { case $1 of { QIdentifier is i -> QIdentifier (i:is) $3 } }

-- Type declarations
Typename
  : BuiltinType { BaseType $1 }
  | Ident       { UserType $1 }
BuiltinType
  : "double"    { PbDouble   }
  | "float"     { PbFloat    }
  | "int32"     { PbInt32    }
  | "int64"     { PbInt64    }
  | "uint32"    { PbUInt32   }
  | "uint64"    { PbUint64   }
  | "sint32"    { PbSInt32   }
  | "sint64"    { PbSint64   }
  | "fixed32"   { PbFixed32  }
  | "fixed64"   { PbFixed64  }
  | "sfixed32"  { PbSFixed32 }
  | "sfixed64"  { PbSFixed64 }
  | "bool"      { PbBool     }
  | "string"    { PbString   }
  | "bytes"     { PbBytes    }

  
{

parseError :: [Token] -> a
parseError _ = error "Parse error"

}