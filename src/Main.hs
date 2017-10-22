module Main where

import Stlc.Types
import Stlc.Lexer
import Stlc.Parser

import Stlc.Constrain
import Stlc.Unify

parse :: String -> Term
parse = parseStlc . alexScanTokens

printAndParse :: [Char] -> IO ()
printAndParse prog = do
  putStrLn $ "> " ++ prog
  print $ parse prog
  putStrLn ""

main :: IO ()
main = do
  printAndParse "0"
  printAndParse "1"
  printAndParse "(0)"
  printAndParse "(True)"
  printAndParse "(False)"
  printAndParse "if True then 0 else 1 end"
  printAndParse "ifz True then 0 else x -> 1 end"
  printAndParse "(\\x -> x)"
