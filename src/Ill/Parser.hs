module Ill.Parser where
import           Control.Applicative    ((<*))

import           Data.List              (intercalate)

import           Text.Megaparsec
import           Text.Megaparsec.Text

import           Ill.Syntax

import           Ill.Parser.Declaration
import           Ill.Parser.Expression
import           Ill.Parser.Lexer

moduleParser :: Parser (Module SourceSpan)
moduleParser = do
  symbol "module"
  name <- intercalate (".") <$> capitalized `sepBy` char '.' <* scn
  body <- many $ declaration <* scn
  symbol "end" <* scn
  return $ Module name body

illParser :: Parser (Module SourceSpan)
illParser = scn *> moduleParser
