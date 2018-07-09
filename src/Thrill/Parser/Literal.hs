module Thrill.Parser.Literal where

import           Thrill.Prelude

import Thrill.Syntax.Literal
import Thrill.Parser.Lexer

import Text.Megaparsec (try)

integerLit :: Parser Literal
integerLit = Integer <$> integer

doubleLit :: Parser Literal
doubleLit = Double <$> double

rawString :: Parser Literal
rawString = RawString <$> str
  where str = dquotes (many $ noneOf "\"")

charLit :: Parser Literal
charLit = Char <$> (squotes printChar)

literal = lexeme $ (try $ doubleLit) <|> integerLit <|> rawString <|> charLit
