{-# LANGUAGE OverloadedStrings #-}
module Ill.Parser
( module Ill.Parser
, parse
, parseErrorPretty
, parseErrorPretty'
, runParser
) where

import           Ill.Prelude

import           Control.Applicative    ((<*))

import           Data.List              (intercalate)

import           Text.Megaparsec

import           Ill.Syntax

import           Ill.Parser.Declaration
import           Ill.Parser.Expression
import           Ill.Parser.Lexer

import qualified Data.Text.IO as T (readFile)
import           Data.Void

moduleParser :: Parser (Module SourceSpan)
moduleParser = do
  symbol "module"
  name <- intercalate (".") <$> capitalized `sepBy` char '.' <* scn
  body <- many $ declaration <* scn
  symbol "end" <* scn
  return $ Module name body

illParser :: Parser (Module SourceSpan)
illParser = scn *> moduleParser

parseFromFile :: Parser a -> FilePath -> IO (Either (ParseError Char Void) a)
parseFromFile p file = runParser p file <$> T.readFile file
