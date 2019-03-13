{-# LANGUAGE OverloadedStrings #-}
module Thrill.Parser.ExpressionSpec where

import SpecHelper

import Test.Hspec
import Text.Megaparsec

import Text.Megaparsec.Pos (SourcePos)
import Test.Hspec.Megaparsec

import Data.List.NonEmpty

import Thrill.Parser.Expression

import Data.Void
import Data.Text (pack)

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
      parse assign "" "a, a = 2" `shouldFailWith` errFancy (posN 8 (pack "12345678")) (fancy $ ErrorFail "Invalid assignment: length mismatch.")
