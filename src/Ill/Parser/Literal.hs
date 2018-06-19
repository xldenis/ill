module Ill.Parser.Literal where

import           Ill.Prelude

import Ill.Syntax.Literal
import Ill.Parser.Lexer

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
