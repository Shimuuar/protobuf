{
module Data.Protobuf.Grammar.Lexer where

import Data.Ratio
}
%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

@decint = [\-]?[1-9][0-9]*
@octint = [\-]?0[0-7]+
@hexint = [\-]?0[Xx][0-9a-fA-F]+
@strlit = (\"[^\"]*\")|('[^']*\')
@ident  = $alpha+

tokens :-
  $white+				;
  @decint               { TokInt . read       }
  @octint               { error "OCTAL"       }
  @hexint               { error "HEX"         }
  @strlit               { TokString . unquote . init . tail }
  @ident                { TokIdent            }
  \{                    { const TokBraceOpen  }
  \}                    { const TokBraceClose }
  \;                    { const TokSemicolon  }
  \=                    { const TokEqual      }
  \.                    { const TokDot        }
{

-- | Token data type  
data Token
  = TokInt    Integer           --  Integer literal
  | TokDouble Rational          --  Floating point literal
  | TokString String            --  String literal
  | TokIdent  String            --  Identifier
  | TokBraceOpen                --  Opening brace {
  | TokBraceClose               --  Closing brace }
  | TokSemicolon                --  Semicolon     ;
  | TokEqual                    --  =
  | TokDot                      --  .
  deriving Show

unquote :: String -> String
unquote = id
}
