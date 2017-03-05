{-# LANGUAGE OverloadedStrings #-}
module Ill.Parser.ExpressionSpec where

import SpecHelper

import Test.Hspec
import Text.Megaparsec

import Text.Megaparsec.Pos (SourcePos)
import Test.Hspec.Megaparsec

import Data.List.NonEmpty

import Ill.Parser.Expression

spec :: Spec
spec = parallel $ do
  unitSpec
  filesShouldParse "test/parser/success/expression" expression
  filesShouldFail  "test/parser/failure/expression" expression

unitSpec :: Spec
unitSpec = do
  describe "call" $ do
    it "terminates" $ do
      shouldSucceed $ parse (call <* eof) "" "func()"
  describe "assign" $ do
    it "errors properly" $ do
      parse assign "" "a, a = 2" `shouldFailWith` err (SourcePos "" (unsafePos 1) (unsafePos 9) :| [])  (cstm $ DecFail "Invalid assignment: length mismatch.")
