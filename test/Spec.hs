{-# LANGUAGE LambdaCase #-}
module Main where

import           Stlc

import           Data.Either
import           Test.Hspec

evalOk :: String -> Expectation
evalOk prog = shouldSatisfy prog (isRight . fst . parseAndEval)

main :: IO ()
main = hspec $ do
  describe "Stlc" $ do
    -- Quick-and-dirty testing: eval it, and check for no exceptions :P
    describe "parse + eval sanity checks" $ do
      it "evaluates function values" $ do
        evalOk "\\x -> x"

      it "evaluates zero values" $ do
        evalOk "0"

      it "evaluates non-zero values" $ do
        evalOk "1"
        evalOk "2"
        evalOk "10"

      it "evaluates boolean values" $ do
        evalOk "True"
        evalOk "False"

      it "evaluates boolean values" $ do
        evalOk "True"
        evalOk "False"

      it "evaluates function application" $ do
        evalOk "(\\x -> x) 0"

      it "evaluates let binding" $ do
        evalOk "let x = 0 in x"

      it "evaluates if-expressions" $ do
        evalOk "if True then 1 else 0"

      it "evaluates ifz-expressions" $ do
        evalOk "ifz 3 then 0 else x -> x"

      it "handles binding correctly" $ do
        evalOk "let x = \\x -> x in x 0"
