module Main where

import           Stlc

import           System.Console.Haskeline (InputT, defaultSettings,
                                           getInputLine, outputStr, runInputT)

repl :: IO ()
repl = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      input <- getInputLine "\nstlc> "
      case input of
        Nothing -> return ()
        Just prog -> do
          outputStr . snd $ parseAndEval prog
          loop

main :: IO ()
main = repl
