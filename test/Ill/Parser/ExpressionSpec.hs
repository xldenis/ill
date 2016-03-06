module Ill.Parser.ExpressionSpec where

import SpecHelper

import Test.Hspec

import Ill.Parser.Expression

spec :: Spec
spec = parallel $ do
  filesShouldParse "test/parser/success/expression" expression
  filesShouldFail  "test/parser/failure/expression" expression

