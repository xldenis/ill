module Thrill.Parser.PatternSpec where
import Test.Hspec
import Text.Megaparsec (many)

import SpecHelper

import Thrill.Parser.Pattern
import Thrill.Parser.Lexer (scn)

spec :: Spec
spec = parallel $ do
  filesShouldParse "test/parser/success/pattern" (many $ pattern <* scn)
