module Main
  ( parse
  , constraints
  , unify
  , principalType
  , inferTerm
  , eval
  , repl
  , main
  ) where

import           Stlc.Constrain (constraints)
import           Stlc.Eval (eval)
import           Stlc.Infer (inferTerm, principalType)
import           Stlc.Parser (parse)
import           Stlc.Unify (unify)

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
              let (t0, cs) = constraints term
              outputStrLn $ "Type: " ++ show t0
              outputStrLn $ "Constraints: " ++ show cs
              case unify cs of
                Just sol -> do
                  outputStrLn $ "Solution: " ++ show sol
                  let t = principalType sol t0
                  outputStrLn $ "Principal type: " ++ show t
                  let val = eval term
                  outputStrLn $ "\n" ++ show val
                Nothing -> do
                  outputStrLn "Unification failed!"
          loop

main :: IO ()
main = repl
