module Main where

import Stlc.Types
import Stlc.Lexer
import Stlc.Parser

import Stlc.Constrain
import Stlc.Unify

parse :: String -> Term
parse = parseStlc . alexScanTokens

main :: IO ()
main = do
  print $ parse "0"
  print $ parse "1"
  print $ parse "(0)"
  print $ parse "(True)"
  print $ parse "(False)"
  print $ parse "if True then 0 else 1 end"
  print $ parse "ifz True then 0 else x -> 1 end"
  print $ parse "(λx -> x)"
