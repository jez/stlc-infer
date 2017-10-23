{
module Stlc.Parser where

import           Stlc.Lexer
import           Stlc.Types
import           Unbound.Generics.LocallyNameless (bind, s2n)

}

%name parseStlc
%tokentype { Token }
%error { parseError }
%monad { Alex }
%lexer { lexer } { TokEOF }

%token
  '\\'       {TokBackslash}
  '->'       {TokThinArrow}
  'let'      {TokLet}
  '='        {TokEq}
  'in'       {TokIn}
  'True'     {TokTrue}
  'False'    {TokFalse}
  'if'       {TokIf}
  'ifz'      {TokIfz}
  'then'     {TokThen}
  'else'     {TokElse}
  '('        {TokLParen}
  ')'        {TokRParen}
  numeral    {TokNumeral $$}
  ident      {TokTermIdent $$}

%%

Term
  : '\\' ident '->' Term   {Tlam (bind (s2n $2) $4)}
  | 'ifz' Term 'then' Term 'else' ident '->' Term  {Tifz $2 $4 (bind (s2n $6) $8)}
  | 'if'  Term 'then' Term 'else'            Term  {Tif $2 $4 $6}
  | 'let' ident '=' Term 'in' Term  {Tlet $4 (bind (s2n $2) $6)}
  | Form                   {$1}

-- TODO(jez) This is here so you remember where to update things
-- if you ever add any infix operators.
-- See http://dev.stephendiehl.com/fun/008_extended_parser.html
Form : Fact {$1}

-- We have to factor the Term grammar into Form and Atom to avoid conflicts
Fact
  : Fact Atom              {Tapp $1 $2}
  | Atom                   {$1}

Atom
  : '(' Term ')'           {$2}
  | 'True'                 {Tbool True}
  | 'False'                {Tbool False}
  | numeral                {unaryFromInt $1}
  | ident                  {Tvar (s2n $1)}

{

unaryFromInt :: Int -> Term
unaryFromInt n | n <= 0 = Tz
unaryFromInt n = Ts . unaryFromInt $ n - 1

parse :: String -> Either String Term
parse s = runAlex s parseStlc

-- TODO(jez) More human readable error messages
parseError :: Token -> Alex a
parseError t = Alex $ Left . (parseErrorMsg t)

parseErrorMsg :: Token -> AlexState -> String
parseErrorMsg t alexState =
  let AlexPn offset line col = alex_pos alexState
      in "Parse error on l" ++ show line ++ ":c" ++ show col ++
         " (char: " ++ show offset ++ "). Token: " ++ show t

}

