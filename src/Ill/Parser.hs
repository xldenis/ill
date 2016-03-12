module Ill.Parser where
  import Control.Applicative ((<*))

  import Data.List (intercalate)

  import Text.Megaparsec.Text
  import Text.Megaparsec

  import Ill.Syntax

  import Ill.Parser.Expression
  import Ill.Parser.Declaration
  import Ill.Parser.Lexer

  ill = expression

  moduleParser :: Parser (Module SourceSpan)
  moduleParser = do
    symbol "module"
    name <- (intercalate (".")) <$> capitalized `sepBy` (char '.') <* scn
    body <- many $ declaration <* scn
    symbol "end" <* scn
    return $ Module name body
