module Ill.Parser.Pattern (pattern) where

import Ill.Syntax
import Ill.Parser.Lexer
import Ill.Parser.Literal

import Text.Megaparsec.Text
import Text.Megaparsec

import Control.Applicative ((<|>))

pattern :: Parser Pattern
pattern = (parens pattern) <|> wildcard <|> destructor <|> pLit <|> var

destructor :: Parser Pattern
destructor = do
  cons <- upperIdent
  args <- many pattern
  return $ Destructor cons args

var :: Parser Pattern
var = PVar <$> identifier

wildcard :: Parser Pattern
wildcard = symbol "_" *> (return Wildcard)

pLit = PLit <$> literal
