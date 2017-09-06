module Ill.Parser.Pattern (pattern) where

import Ill.Syntax
import Ill.Parser.Lexer
import Ill.Parser.Literal

import Text.Megaparsec.Text
import Text.Megaparsec

import Control.Applicative ((<|>))

pattern :: Parser (Pat SourceSpan)
pattern = (parens pattern) <|> wildcard <|> destructor <|> pLit <|> var

destructor :: Parser (Pat SourceSpan)
destructor = withLoc $ do
  cons <- upperIdent
  args <- many pattern
  return $ Destructor cons args

var :: Parser (Pat SourceSpan)
var = withLoc $ PVar <$> identifier

wildcard :: Parser (Pat SourceSpan)
wildcard = withLoc $ symbol "_" *> (return Wildcard)

pLit = withLoc $ PLit <$> literal
