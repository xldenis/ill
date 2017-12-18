{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Ill.Parser
import Ill.Syntax.Pretty
import Ill.Syntax (Module(..))
import Ill.Parser.Lexer (SourceSpan)

import qualified Data.Text.Lazy.IO as T (putStrLn)
import qualified Data.Text.IO as T (readFile)

import Infer
import DesugarDebug
import Interpreter
import CoreDebug

import Options.Applicative.Simple

import Paths_ill

data Build = Build String
data Format = Format String
data Infer = Infer String
data Desugar = Desugar String String
data Run = Run String
data Core = Core String (Maybe String)

fileArg = strArgument (metavar "FILE" <> help "location of source file")
stageArg = strArgument (metavar "STAGE")
filterArg = optional . strOption $ long "filter" <> metavar "FILTER" <> short 'f'
  <> help "only print the binding that exactly matches the filter provided"

globalFlags = flag True False (long "no-default-prelude" <> help "Disable the implicit prelude module")

options = do
  simpleOptions "v0.0.1" "ill: lol" "omg" (globalFlags) $ do
    addCommand "format"
      "prettyprint the module at a given location"
      format (Format <$> fileArg)
    addCommand "infer"
      "run the typechecker and output information about methods and traits"
      inferC (Infer <$> fileArg)
    addCommand "run"
      "execute the main function in a module using a lazy interpreter"
      run    (Run <$> fileArg)
    addCommand "core"
      "generate core bindings for a module"
      core   (Core <$> fileArg <*> filterArg)
    addCommand "desugar"
      "view the desugaring pipeline up to a given stage"
      desugarC (Desugar <$> stageArg <*> fileArg)

format (Format f) = commandWrapper f (T.putStrLn . renderIll defaultRenderArgs . pretty)
inferC (Infer f)  = commandWrapper f (infer)
run    (Run f)    = commandWrapper f (runInterpreter)
core   (Core f filter) = commandWrapper f (coreDebug filter)
desugarC (Desugar s f) = commandWrapper f (desugar s)

commandWrapper file com parsedPrelude = do
  stream <- T.readFile file
  let parsed = runParser illParser (file) stream
  let joined = (,) <$> parsedPrelude <*> parsed
  case joined of
    Left err -> putStrLn $ parseErrorPretty' stream err
    Right (prelude, ast) -> com (mergeModules prelude ast)

main :: IO ()
main = do
  (opts, cmd) <- options

  parsedPrelude <- case opts of
    True -> do
      preludePath <- getDataFileName "assets/prelude.ill"
      parseFromFile moduleParser preludePath
    False -> pure $ pure (Module "Prelude" [])
  cmd parsedPrelude

mergeModules (Module _ ds) (Module n ds2) = Module n (ds ++ ds2)
