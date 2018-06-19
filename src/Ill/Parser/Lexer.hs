{-# LANGUAGE OverloadedStrings #-}
module Ill.Parser.Lexer
( module Ill.Parser.Lexer
, SourceSpan(..)
, module Text.Megaparsec.Char
, pack, unpack, Text
) where

import           Ill.Prelude

import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char
import Text.Megaparsec hiding (many)

import Control.Comonad.Cofree

import Data.Text (Text, unpack, pack)

import Data.Void

import Ill.Syntax (SourceSpan(..))

type Parser = Parsec Void Text

reserved :: [String]
reserved = ["if", "then", "else", "end", "fn", "import", "qualified", "hiding", "trait", "data", "type", "or", "case", "of", "when"]

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

identifier :: Parser String
identifier = p >>= res
  where p = label "identifier" . lexeme $ ((:) <$> lowerChar <*> many identLetters)
        res i = if i `elem` reserved then
            fail $ "The reserved word `" ++ i ++ "` cannot be used as an identifier."
          else
            return i

identLetters = oneOf letters
  where
  letters :: String
  letters = "_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"

upperIdent :: Parser String
upperIdent = lexeme capitalized

capitalized :: Parser String
capitalized = (:) <$> upperChar <*> many identLetters

scn :: Parser ()
scn = L.space (void spaceChar) lineComment empty

sc :: Parser ()
sc = L.space (void $ oneOf [' ', '\t']) lineComment empty

sep :: Parser ()
sep = oneOf ['\n', ';'] *> scn

lineComment :: Parser ()
lineComment = char '#' *> skipMany (noneOf ['\n'])

integer :: Parser Integer
integer = lexeme (L.signed sc L.decimal)

double :: Parser Double
double = lexeme (L.signed sc L.float)

withLoc :: Parser (a (Cofree a SourceSpan)) -> Parser (Cofree a SourceSpan)
withLoc p = do
  beg <- getPosition
  body <- p
  end <- getPosition
  return $ SourceSpan beg end :< body

list :: Parser a -> Parser [a]
list a = a `sepBy` symbol ","

parens :: Parser a -> Parser a
parens = between (symbol "(" :: Parser Text) (symbol ")" :: Parser Text)

squotes :: Parser a -> Parser a
squotes = between (char '\'') (char '\'')

dquotes :: Parser a -> Parser a
dquotes = between (char '"') (char '"')
