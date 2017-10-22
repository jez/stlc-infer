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

  [\\]                           {\_ _ -> TokBackslash}
  "->"                           {\_ _ -> TokThinArrow}
  "True"                         {\_ _ -> TokTrue}
  "False"                        {\_ _ -> TokFalse}
  "if"                           {\_ _ -> TokIf}
  "ifz"                          {\_ _ -> TokIfz}
  "then"                         {\_ _ -> TokThen}
  "else"                         {\_ _ -> TokElse}
  -- TODO(jez) Replace 'end' with significant whitespace
  "end"                          {\_ _ -> TokEnd}
  [\(]                           {\_ _ -> TokLParen}
  [\)]                           {\_ _ -> TokRParen}
  "="                            {\_ _ -> TokEq}
  $digit+                        {\_ -> TokNumeral . read}
  $lower [$alpha $digit \_ \']*  {\_ -> TokTermIdent}

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
  | TokEnd
  | TokLParen
  | TokRParen
  | TokEq
  | TokNumeral Int
  | TokTermIdent String
  deriving (Eq, Show)
}
