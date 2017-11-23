{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Ill.Parser
import Ill.Syntax.Pretty
import Ill.Syntax (Module(..))
import Ill.Parser.Lexer (SourceSpan)

import Text.Megaparsec

import Options.Generic

import qualified Data.Text.Lazy.IO as T (putStrLn)
import qualified Data.Text.IO as T (readFile)

import Infer
import DesugarDebug
import Interpreter
import CoreDebug

import Paths_ill

data Config
  = Build String
  | Format String
  | Infer String
  | Desugar String String
  | Run String
  | Core String
  deriving (Generic, Show)

instance ParseRecord Config


file (Build f) = f
file (Format f) = f
file (Infer f) = f
file (Desugar _ f) = f
file (Run f) = f
file (Core f) = f

main :: IO ()
main = do
  config <- getRecord "Ill Compiler" :: IO Config
  preludePath <- getDataFileName "assets/prelude.ill"
  parsedPrelude <- parseFromFile moduleParser preludePath

  stream <- T.readFile (file config)
  let parsed = runParser illParser (file config) stream

  let joined = (,) <$> parsedPrelude <*> parsed

  case joined of
    Left err -> putStrLn $ parseErrorPretty' stream err
    Right (prelude, ast) -> handleCommands config (mergeModules prelude ast)

mergeModules (Module _ ds) (Module n ds2) = Module n (ds ++ ds2)

handleCommands :: Config -> Module SourceSpan -> IO ()
handleCommands (Build f)  ast = putStrLn "build is not implemented yet, use run to interpret code"
handleCommands (Format f) ast = T.putStrLn $ renderIll defaultRenderArgs (pretty ast)
handleCommands (Infer f)  ast = infer ast
handleCommands (Desugar stage f) ast = desugar stage ast
handleCommands (Run f) ast = runInterpreter ast
handleCommands (Core f) ast = coreDebug ast
