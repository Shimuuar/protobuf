{
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports     #-}
{-# OPTIONS_GHC -fno-warn-unused-binds       #-}
{-# OPTIONS_GHC -fno-warn-unused-matches     #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Data.Protobuf.Grammar.Lexer (
    Token(..)
  , scanTokens
  ) where

import Text.Printf
}
%wrapper "monad"

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
  @decint               { si $ TokInt . read }
  @octint               { error "OCTAL"      }
  @hexint               { error "HEX"        }
  @strlit               { si $ TokString . unquote . init . tail }
  @ident                { si $ TokIdent    }
  \{                    { co TokBraceOpen  }
  \}                    { co TokBraceClose }
  \[                    { co TokSBrkOpen   }
  \]                    { co TokSBrkClose  }
  \;                    { co TokSemicolon  }
  \=                    { co TokEqual      }
  \.                    { co TokDot        }
  \,                    { co TokComma      }
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
  | TokEOF                      --  End of file token
  deriving (Show,Eq)

scanTokens :: FilePath -> String -> Either String [Token]
scanTokens fname str = runAlex str scan
  where
    scan = do
      t <- alexCustomScan fname
      case t of
        TokEOF -> return []
        _      -> do ts <- scan
                     return (t : ts)

-- Custom scanner. Slightly modified version of alexMonadScan
alexCustomScan :: FilePath -> Alex Token
alexCustomScan fname = do
  inp <- alexGetInput
  sc  <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError ((AlexPn _ ln col),_,_,_) -> alexError $ printf "Lexical error in %s %i:%i" fname ln col
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexCustomScan fname
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len


alexEOF = return TokEOF


-- Remove quotations in the string literals
-- FIXME: implement
unquote :: String -> String
unquote = id

-- Simple parser
si :: (String -> r) -> (AlexPosn,Char,String) -> Int -> Alex r
si f (_,_,tok) n = return (f $ take n tok)

-- Constant expression
co :: r -> (AlexPosn,Char,String) -> Int -> Alex r
co x _ _ = return x
}
