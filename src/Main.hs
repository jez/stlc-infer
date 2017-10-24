module Main where

import           Stlc.Constrain
import           Stlc.Eval
import           Stlc.Infer
import           Stlc.Parser
import           Stlc.Unify

import           System.Console.Haskeline (InputT, defaultSettings,
                                           getInputLine, outputStrLn, runInputT)

repl :: IO ()
repl = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      input <- getInputLine "\nstlc> "
      case input of
        Nothing -> return ()
        Just prog -> do
          case parse prog of
            Left msg -> outputStrLn msg
            Right term -> do
              outputStrLn $ "Term: " ++ show term
              let (t0, cs) = constraintsWithCon term
              outputStrLn $ "Type: " ++ show t0
              outputStrLn $ "Constraints: " ++ show cs
              case unify cs of
                Just sol -> do
                  outputStrLn $ "Solution: " ++ show sol
                  let t = principalType sol t0
                  outputStrLn $ "Principal type: " ++ show t
                  let val = runEval term
                  outputStrLn $ "\n" ++ show val
                Nothing -> do
                  outputStrLn "Unification failed!"
          loop

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
  printAndParse "True"
  printAndParse "False"
  printAndParse "if True then 0 else 1"
  printAndParse "ifz True then 0 else x -> 1"
  printAndParse "\\x -> x"
  printAndParse "let x = True in x"
  printAndParse "let x = True in let y = False in z"
  printAndParse "let id = \\x -> x in id id"
  -- Uncomment to test lexer error
  -- printAndParse "λx -> x"
  -- Uncomment to test parse error
  -- printAndParse "\\x ->"
