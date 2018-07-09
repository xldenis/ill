{-# LANGUAGE OverloadedStrings #-}
module Thrill.Parser.Pattern (pattern) where

import Thrill.Prelude

import Thrill.Syntax
import Thrill.Parser.Lexer
import Thrill.Parser.Literal

import Text.Megaparsec (try, label)

pattern :: Parser (Pat' Name SourceSpan)
pattern = label "pattern" $ (parens pattern) <|> wildcard <|> destructor <|> pLit <|> var

destructor :: Parser (Pat' Name SourceSpan)
destructor = label "destructor" . withLoc $ do
  cons <- upperIdent
  args <- many $ simpleDestructor <|> pLit <|> var <|> wildcard <|> parens pattern
  return $ Destructor cons args
  where
  simpleDestructor = withLoc $ Destructor <$> upperIdent <*> pure []

var :: Parser (Pat' Name SourceSpan)
var = label "variable" . withLoc $ PVar <$> identifier

wildcard :: Parser (Pat' Name SourceSpan)
wildcard = label "wildcard" $ withLoc $ symbol "_" *> (return Wildcard)

pLit = label "literal" . withLoc $ PLit <$> literal
