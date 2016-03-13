module Ill.Parser.TypeSpec where
  import Test.Hspec
  import Text.Megaparsec (many)

  import SpecHelper

  import Ill.Parser.Type
  import Ill.Parser.Lexer (scn)

  spec :: Spec
  spec = parallel $ do
    filesShouldParse "test/parser/success/type" (many $ typeExp <* scn)
