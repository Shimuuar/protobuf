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
  "double"   { TokDouble $$  }
  "string"   { TokString $$  }
  "ident"    { TokIdent  $$  }
  -- Punctuation
  "{"        { TokBraceOpen  }
  "}"        { TokBraceClose }
  ";"        { TokSemicolon  }
  "."        { TokDot        }
  "="        { TokEqual      }
  -- Reserved words
  "message"  { TokIdent "message" }
  "import"   { TokIdent "import"  }
  "option"   { TokIdent "option"  }
  "enum"     { TokIdent "enum"    }
  "package"  { TokIdent "package" }
  "extend"   { TokIdent "extend"  }
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
  : "import" "string" ";"     { Import $2 }
Message
  : "message" Ident "{" "}" { MessageDecl (Message $2 []) }
-- FIXME: option value could be almost anything!
Option
  : "option"  QIdent "=" "string" { TopOption (Option $2 $4) }
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

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

}