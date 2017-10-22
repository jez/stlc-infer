{
module Stlc.Lexer where
}

%wrapper "basic"

$digit = [0-9]

$lower = [a-z]
$alpha = [A-Za-z]

tokens :-
  -- Whitespace insensitive
  $white+                        ;

  -- Comments
  "--".*                         ;

  [\\]                           {\_ -> TokBackslash}
  "->"                           {\_ -> TokThinArrow}
  "True"                         {\_ -> TokTrue}
  "False"                        {\_ -> TokFalse}
  "if"                           {\_ -> TokIf}
  "ifz"                          {\_ -> TokIfz}
  "then"                         {\_ -> TokThen}
  "else"                         {\_ -> TokElse}
  -- TODO(jez) Replace 'end' with significant whitespace
  "end"                          {\_ -> TokEnd}
  [\(]                           {\_ -> TokLParen}
  [\)]                           {\_ -> TokRParen}
  "="                            {\_ -> TokEq}
  $digit+                        {TokNumeral . read}
  $lower [$alpha $digit \_ \']*  {TokTermIdent}

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
