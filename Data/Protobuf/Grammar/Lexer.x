{
module Data.Protobuf.Grammar.Lexer (
    Token(..)
  , alexScanTokens
  ) where

import Data.Ratio
}
%wrapper "basic"

$digit = 0-9			-- digits

@decint   = [\-]?[1-9][0-9]*
@octint   = [\-]?0[0-7]+
@hexint   = [\-]?0[Xx][0-9a-fA-F]+
@strlit   = (\"[^\"]*\")|('[^']*\')
@ident    = [a-zA-Z][a-zA-Z_0-9]*
@comment  = \/\/[^\n]*
@comment2 = \/\*.*\*\/

tokens :-
  $white+               ;
  @comment              ;
  @comment2             ;
  @decint               { TokInt . read       }
  @octint               { error "OCTAL"       }
  @hexint               { error "HEX"         }
  @strlit               { TokString . unquote . init . tail }
  @ident                { TokIdent            }
  \{                    { const TokBraceOpen  }
  \}                    { const TokBraceClose }
  \[                    { const TokSBrkOpen   }
  \]                    { const TokSBrkClose  }
  \;                    { const TokSemicolon  }
  \=                    { const TokEqual      }
  \.                    { const TokDot        }
  \,                    { const TokComma      }
{

-- | Token data type  
data Token
  = TokInt    Integer           --  Integer literal
  | TokDouble Rational          --  Floating point literal
  | TokString String            --  String literal
  | TokIdent  String            --  Identifier
  | TokBraceOpen                --  Opening brace {
  | TokBraceClose               --  Closing brace }
  | TokSBrkOpen                 --  Opening square bracket [
  | TokSBrkClose                --  Closing square bracket ]
  | TokSemicolon                --  Semicolon     ;
  | TokEqual                    --  =
  | TokDot                      --  .
  | TokComma                    --  ,
  deriving Show

-- Remove quotations in the string literals
-- FIXME: implement
unquote :: String -> String
unquote = id

}
