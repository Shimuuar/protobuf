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
@strlit = 

tokens :-
  $white+				;
  @decint               { TokInt . read }
  @octint               { error "OCTAL" }
  @hexint               { error "HEX"   }
  
{

-- | Token data type  
data Token
  = TokInt    Integer           --  Integer literal
  | TokDouble Rational          --  Floating point literal
  | TokString String            --  String literal
  | TokIdent  String            --  Identifier

}
