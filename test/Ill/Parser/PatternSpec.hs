module Ill.Parser.PatternSpec where
import Test.Hspec
import Text.Megaparsec (many)

import SpecHelper

import Ill.Parser.Pattern
import Ill.Parser.Lexer (scn)

spec :: Spec
spec = parallel $ do
  filesShouldParse "test/parser/success/pattern" (many $ pattern <* scn)
