{-# LANGUAGE OverloadedStrings #-}
module Thrill.Parser
( module Thrill.Parser
, parse
, parseErrorPretty
, parseErrorPretty'
, runParser
, ParseError
) where

import           Thrill.Prelude

import           Text.Megaparsec hiding (many)

import           Thrill.Syntax

import           Thrill.Parser.Declaration
import           Thrill.Parser.Lexer

import qualified Data.Text.IO as T (readFile)
import           Data.Void

moduleParser :: Parser (Module Name SourceSpan)
moduleParser = do
  symbol "module"
  name <- intercalate (".") <$> capitalized `sepBy` char '.' <* scn
  body <- manyTill (declaration <* scn) (label "end of module" $ symbol "end" <* scn)
  return $ Module name body

thrillParser :: Parser (Module Name SourceSpan)
thrillParser = scn *> moduleParser

parseFromFile :: Parser a -> FilePath -> IO (Either (ParseError Char Void) a)
parseFromFile p file = runParser p file <$> T.readFile file

