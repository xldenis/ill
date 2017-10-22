{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Infer
import Ill.Parser
import Ill.Syntax.Pretty
import DesugarDebug
import Interpreter
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
  | Run String
  deriving (Generic, Show)

instance ParseRecord Config


file (Build f) = f
file (Format f) = f
file (Infer f) = f
file (Desugar _ f) = f
file (Run f) = f

main :: IO ()
main = do
  config <- getRecord "Ill Compiler" :: IO Config
  parsed <- parseFromFile illParser (file config)
  case parsed of
    Left err -> putStrLn $ parseErrorPretty err
    Right ast -> handleCommands config ast

handleCommands :: Config -> Module SourceSpan -> IO ()
handleCommands (Build f)  ast = putStrLn "build is not implemented yet, use run to interpret code"
handleCommands (Format f) ast = T.putStrLn $ renderIll defaultRenderArgs (pretty ast)
handleCommands (Infer f)  ast = infer ast
handleCommands (Desugar stage f) ast = desugar stage ast
handleCommands (Run f) ast = runInterpreter ast
