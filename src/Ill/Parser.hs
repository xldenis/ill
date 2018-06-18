{-# LANGUAGE OverloadedStrings #-}
module Ill.Parser
( module Ill.Parser
, parse
, parseErrorPretty
, parseErrorPretty'
, runParser
, ParseError
) where

import           Ill.Prelude

import           Text.Megaparsec hiding (many)

import           Ill.Syntax

import           Ill.Parser.Declaration
import           Ill.Parser.Lexer

import qualified Data.Text.IO as T (readFile)
import           Data.Void
import           Ill.Error
import           Data.Bifunctor (first)

moduleParser :: Parser (Module Name SourceSpan)
moduleParser = do
  symbol "module"
  name <- intercalate (".") <$> capitalized `sepBy` char '.' <* scn
  imports <-  many $ importDeclaration <* scn
  body <- many $ declaration <* scn
  symbol "end" <* scn
  return $ Module name imports body


importDeclaration :: Parser Import
importDeclaration = label "import" $ do
  symbol "import"
  qual <- qualified
  path <- intercalate "." <$> (lexeme $ capitalized `sepBy` (char '.'))
  alias <- if qual then Just <$> alias else optional alias
  imports <- mask

  return $ Import qual imports path alias
  where qualified = isJust <$> (optional $ symbol "qualified")
        alias = symbol "as" *> upperIdent
        mask = (try $ do
          cons <- (symbol "hiding" *> return Hiding) <|> (return Only)
          args <- parens $ list identifier
          return $ cons args) <|> (return All)

illParser :: Parser (Module Name SourceSpan)
illParser = scn *> moduleParser

parseFromFile :: Parser a -> FilePath -> IO (Either (Error ann) a)
parseFromFile p file = do
  stream <- T.readFile file
  let result = runParser p file stream

  return $ first (fromParseError stream) result
  where
  fromParseError stream err = Error
    { errKind = "parser"
    , errSummary = pretty $ parseErrorPretty' stream err
    }
