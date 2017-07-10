{-# LANGUAGE OverloadedStrings #-}
module Ill.Parser.TypeSpec where
import Test.Hspec
import Text.Megaparsec (many, parse)
import Test.Hspec.Megaparsec
import SpecHelper

import Ill.Parser.Type
import Ill.Parser.Lexer (scn)
import Ill.Syntax.Type

spec :: Spec
spec = parallel $ do
  filesShouldParse "test/parser/success/type" (many $ typeExp <* scn)
  it "should parse parameterized types in functions" $ do
    parse fullType "" "f b -> f c" `shouldParse` ((TVar "f" `TAp` TVar "b" ) `Arrow` (TVar "f" `TAp` TVar "c" ))