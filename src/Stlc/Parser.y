{
module Stlc.Parser where

import           Stlc.Lexer
import           Stlc.Types
import           Unbound.Generics.LocallyNameless (bind, s2n)

}

%name parseStlc
%tokentype {Token}
%error {const (error "Parse error")}

%token
  '\\'       {TokBackslash}
  '->'       {TokThinArrow}
  'True'     {TokTrue}
  'False'    {TokFalse}
  'if'       {TokIf}
  'ifz'      {TokIfz}
  'then'     {TokThen}
  'else'     {TokElse}
  'end'      {TokEnd}
  '('        {TokLParen}
  ')'        {TokRParen}
  '='        {TokEq}
  numeral    {TokNumeral $$}
  ident      {TokTermIdent $$}

%%

Term
  : ident {Tvar (s2n $1)}

  | '(' '\\' ident '->' Term ')' {Tlam (bind (s2n $3) $5)}
  | Term '(' Term ')' {Tapp $1 $3}

  | numeral {unaryFromInt $1}
  | 'ifz' Term 'then' Term 'else' ident '->' Term 'end' {Tifz $2 $4 (bind (s2n $6) $8)}

  | 'True' {Tbool True}
  | 'False' {Tbool False}
  | 'if' Term 'then' Term 'else' Term 'end' {Tif $2 $4 $6}

  | '(' Term ')' {$2}

{

unaryFromInt :: Int -> Term
unaryFromInt n | n <= 0 = Tz
unaryFromInt n = Ts . unaryFromInt $ n - 1

}

