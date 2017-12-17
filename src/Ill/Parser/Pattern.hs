{-# LANGUAGE OverloadedStrings #-}
module Ill.Parser.Pattern (pattern) where

import Ill.Prelude

import Ill.Syntax
import Ill.Parser.Lexer
import Ill.Parser.Literal

import Text.Megaparsec

import Control.Applicative ((<|>))

pattern :: Parser (Pat SourceSpan)
pattern = (parens pattern) <|> wildcard <|> destructor <|> pLit <|> var

destructor :: Parser (Pat SourceSpan)
destructor = withLoc $ do
  cons <- upperIdent
  args <- many $ simpleDestructor <|> pLit <|> var <|> wildcard <|> parens pattern
  return $ Destructor cons args
  where
  simpleDestructor = withLoc $ Destructor <$> upperIdent <*> pure []

var :: Parser (Pat SourceSpan)
var = withLoc $ PVar <$> identifier

wildcard :: Parser (Pat SourceSpan)
wildcard = withLoc $ symbol "_" *> (return Wildcard)

pLit = withLoc $ PLit <$> literal
