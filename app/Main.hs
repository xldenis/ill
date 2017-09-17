{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Infer
import Ill.Parser
import Ill.Syntax.Pretty
import DesugarDebug

import Text.Megaparsec

import Ill.Syntax (Module)
import Ill.Parser.Lexer (SourceSpan)

import Options.Generic

import qualified Data.Text.Lazy.IO as T (putStrLn)
import qualified Data.Text.IO as T (readFile)

data Config
  = Build String
  | Format String
  | Infer String
  | Desugar String String
  deriving (Generic, Show)

instance ParseRecord Config


file (Build f) = f
file (Format f) = f
file (Infer f) = f
file (Desugar _ f) = f

parseFromFile p file = runParser p file <$> T.readFile file

main :: IO ()
main = do
  config <- getRecord "Ill Compiler" :: IO Config
  parsed <- parseFromFile illParser (file config)
  case parsed of
    Left err -> putStrLn $ parseErrorPretty err
    Right ast -> handleCommands config ast

handleCommands :: Config -> Module SourceSpan -> IO ()
handleCommands (Build f)  ast = putStrLn "build"
handleCommands (Format f) ast = T.putStrLn $ renderIll defaultRenderArgs (pretty ast)
handleCommands (Infer f)  ast = infer ast
handleCommands (Desugar stage f) ast = desugar stage ast
