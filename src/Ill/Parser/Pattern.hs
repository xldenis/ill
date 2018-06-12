{-# LANGUAGE OverloadedStrings #-}
module Ill.Parser.Pattern (pattern) where

import Ill.Prelude

import Ill.Syntax
import Ill.Parser.Lexer
import Ill.Parser.Literal

import Text.Megaparsec (try)

pattern :: Parser (Pat' Name SourceSpan)
pattern = (parens pattern) <|> wildcard <|> destructor <|> pLit <|> var

destructor :: Parser (Pat' Name SourceSpan)
destructor = withLoc $ do
  cons <- upperIdent
  args <- many $ simpleDestructor <|> pLit <|> var <|> wildcard <|> parens pattern
  return $ Destructor cons args
  where
  simpleDestructor = withLoc $ Destructor <$> upperIdent <*> pure []

var :: Parser (Pat' Name SourceSpan)
var = withLoc $ PVar <$> identifier

wildcard :: Parser (Pat' Name SourceSpan)
wildcard = withLoc $ symbol "_" *> (return Wildcard)

pLit = withLoc $ PLit <$> literal
