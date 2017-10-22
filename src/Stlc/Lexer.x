{
module Stlc.Lexer where
}

-- The posn wrapper tracks line and column information for us.
-- This makes the error message from the lexer friendlier.
-- TODO(jez) Track the posn information to print better type errors.
%wrapper "posn"

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
  deriving (Eq, Show)

-- Useful combinators for constructing token actions
const2 :: a -> b -> c -> a
const2 = const.const

withString :: (String -> a) -> AlexPosn -> String -> a
withString f _ s = f s

}
