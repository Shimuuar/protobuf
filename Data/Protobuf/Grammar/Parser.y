{
module Data.Protobuf.Grammar.Parser (
  parseProtobuf
  ) where

import Data.Protobuf.Grammar.Lexer
import Data.Protobuf.AST
}

%name      parseProtobuf Protobuf
%tokentype { Token }
%error     { parseError }

%token
  -- Punctuation
  "{"        { TokBraceOpen  }
  "}"        { TokBraceClose }
  ";"        { TokSemicolon  }
  "."        { TokDot        }
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
  "int"      { TokInt    $$  }
  "real"     { TokDouble $$  }
  "strlit"   { TokString $$  }
  "ident"    { TokIdent  $$  }


%%

Protobuf :: { ProtobufFile () }
  : ProtobufDecls       { ProtobufFile $1 [] () (Global ()) }

-- Complete protobuf file
ProtobufDecls
  : {- empty -}               { [] }
  | Declaration ProtobufDecls { $1 : $2 }
  | ";"         ProtobufDecls { $2 }

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
  : "message" Ident "{" MessageFields "}" { Message $2 $4 () }
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
  : Modifier Typename Ident "=" "int" ";" { Field $1 $2 $3 (FieldTag $5) [] }
Modifier
  : "required" { Required }
  | "optional" { Optional }
  | "repeated" { Repeated }

-- Enumeration
Enum
  : "enum" Ident "{" EnumFields "}"   { EnumDecl $2 $4 }
EnumFields
  : EnumField            { $1 : [] }
  | EnumField EnumFields { $1 : $2 }
EnumField
  : Option               { EnumOption $1    }
  | Ident "=" "int" ";"  { EnumField  $1 $3 }
  
-- FIXME: option value could be almost anything!
Option
  : "option"  FullQualId "=" "strlit" { Option $2 $4 }
Package
  : "package" QIdent ";" { Package $2 }
-- FIXME:
Extend
  : "extend"  { Extend  undefined undefined }



-- Identifier
Ident
  : "ident"          { Identifier $1 }
-- Identifier which could be fully qualified
FullQualId
  : "." QualifiedId  { case $2 of 
                         QualId q n -> FullQualId q n 
                         _          -> error "Impossible happened: FullQualId"
                     }
  | QualifiedId      { $1 }
-- Identifier which couldn't be full qualified
QualifiedId
  : QIdent           { case $1 of
                         [] -> error "Impossible happened: PackageId" 
                         xs -> QualId (init xs) (last xs)
                     }
-- Worker for qulified identifiers
QIdent 
  : Ident            { [$1] }
  | Ident "." QIdent { $1 : $3 }

-- Type declarations
Typename
  : BuiltinType { BaseType $1 }
  | FullQualId  { UserType $1 }
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

}