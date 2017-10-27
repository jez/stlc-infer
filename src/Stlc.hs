module Stlc
  ( parse
  , constraints
  , unify
  , principalType
  , inferTerm
  , eval
  , parseAndEval
  ) where

import           Stlc.Types
import           Stlc.Parser (parse)
import           Stlc.Constrain (constraints)
import           Stlc.Unify (unify)
import           Stlc.Infer (inferTerm, principalType)
import           Stlc.Eval (eval)

import           Control.Monad.Writer


tellLn :: String -> Writer String ()
tellLn msg = tell $ msg ++ "\n"

parseAndEvalWriter :: String -> Writer String (Either String Term)
parseAndEvalWriter prog = do
  case parse prog of
    Left msg -> do
      tell msg
      return $ Left msg
    Right term -> do
      tellLn $ "Term: " ++ show term
      let (t0, cs) = constraints term
      tellLn $ "Type: " ++ show t0
      tellLn $ "Constraints: " ++ show cs
      case unify cs of
        Just sol -> do
          tellLn $ "Solution: " ++ show sol
          let t = principalType sol t0
          tellLn $ "Principal type: " ++ show t
          let val = eval term
          tellLn $ "\n" ++ show val
          return $ Right val
        Nothing -> do
          let msg = "Unification failed!"
          tellLn msg
          return $ Left msg

-- Returns the result of parsing and evaling,
-- along with any debugging output produced in the process.
parseAndEval :: String -> (Either String Term, String)
parseAndEval = runWriter . parseAndEvalWriter

