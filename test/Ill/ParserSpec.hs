module Ill.ParserSpec where

import Control.Applicative ((<*))
import Text.Megaparsec (many)

import Ill.Parser.Lexer (scn)
import Ill.Parser
import Ill.Parser.Declaration

import Test.Hspec

import SpecHelper

spec :: Spec
spec = parallel $ do
  filesShouldParse "test/parser/success" moduleParser
  filesShouldParse "test/parser/success/declaration" (many $ declaration <* scn)
  filesShouldFail  "test/parser/failure/declaration" (declaration)
