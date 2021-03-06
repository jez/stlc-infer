{
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Stlc.Lexer where
}

-- The lexer generator for our language.
-- Alex will generate a Haskell program from this
-- specification as a build step (see the cabal file).
--
-- We try to emulate Haskell syntax as much as possible.

-- The posn wrapper tracks line and column information for us.
-- This makes the error message from the lexer friendlier.
-- TODO(jez) Track the posn information to print better type errors.
%wrapper "monad"

$digit = [0-9]

$lower = [a-z]
$alpha = [A-Za-z]

tokens :-
  -- Whitespace insensitive
  $white+                        ;

  -- Comments
  "--".*                         ;

  [\\]                           {const2 TokBackslash}
  "->"                           {const2 TokThinArrow}
  "let"                          {const2 TokLet}
  "="                            {const2 TokEq}
  "in"                           {const2 TokIn}
  "True"                         {const2 TokTrue}
  "False"                        {const2 TokFalse}
  "if"                           {const2 TokIf}
  "ifz"                          {const2 TokIfz}
  "then"                         {const2 TokThen}
  "else"                         {const2 TokElse}
  [\(]                           {const2 TokLParen}
  [\)]                           {const2 TokRParen}
  -- TODO(jez) Replace naturals with integers
  $digit+                        {withString $ TokNumeral . read}
  $lower [$alpha $digit \_ \']*  {withString $ TokTermIdent}

{
data Token
  = TokBackslash
  | TokThinArrow
  | TokLet
  | TokEq
  | TokIn
  | TokTrue
  | TokFalse
  | TokIf
  | TokIfz
  | TokThen
  | TokElse
  | TokLParen
  | TokRParen
  | TokNumeral Int
  | TokTermIdent String
  | TokEOF
  deriving (Eq, Show)

alexEOF :: Alex Token
alexEOF = return TokEOF

-- Useful combinators for constructing token actions
const2 :: a -> b -> c -> Alex a
const2 = const.const.return

withString :: (String -> a) -> AlexInput -> Int -> Alex a
withString f (_, _, _, input) len = return . f $ take len input

lexer :: (Token -> Alex a) -> Alex a
lexer = (alexMonadScan >>=)

}
