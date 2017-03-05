module Ill.Parser.Lexer where

import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec.Text
import Text.Megaparsec

import Control.Comonad.Cofree
import Control.Applicative ((<*), empty)
import Control.Monad (void)

data SourceSpan = SourceSpan {begin :: SourcePos, end :: SourcePos} deriving (Eq, Show)

reserved :: [String]
reserved = ["if", "then", "else", "end", "fn", "import", "qualified", "hiding", "trait", "data", "type", "or"]

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

identifier :: Parser String
identifier = p >>= res
  where p = label "identifier" . lexeme $ ((:) <$> letterChar <*> many alphaNumChar)
        res i = if i `elem` reserved then
            fail $ "The reserved word `" ++ i ++ "` cannot be used as an identifier."
          else
            return i

upperIdent :: Parser String
upperIdent = lexeme capitalized

capitalized :: Parser String
capitalized = (:) <$> upperChar <*> many alphaNumChar

scn :: Parser ()
scn = L.space (void spaceChar) lineComment empty

sc :: Parser ()
sc = L.space (void $ oneOf " \t") lineComment empty

sep :: Parser ()
sep = oneOf "\n;" *> scn

lineComment :: Parser ()
lineComment = char '#' *> skipMany (noneOf "\n")

integer :: Parser Integer
integer = lexeme (L.signed sc L.integer)

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
parens = between (symbol "(") (symbol ")")

squotes :: Parser a -> Parser a
squotes = between (char '\'') (char '\'')
