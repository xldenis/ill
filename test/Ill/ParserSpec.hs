module Ill.ParserSpec where

import Ill.Parser

import Test.Hspec

import SpecHelper

spec :: Spec
spec = do
  filesShouldParse "test/parser/success" ill
