{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Thrill.Parser as Parser
import Thrill.Syntax.Pretty
import Thrill.Syntax (Module, Module'(..), moduleApply)
import Thrill.Parser.Lexer (SourceSpan)

import qualified Data.Text.Lazy.IO as T (putStrLn)
import qualified Data.Text.IO as T (readFile)

import Infer
import DesugarDebug
import Interpreter
import CoreDebug
import CodegenDebug
import Compile
import Thrill.Options
import Thrill.Renamer
import Thrill.BindingGroup
import Thrill.Error
import Thrill.Syntax.Name (QualifiedName)

import Options.Applicative.Simple
import Data.Void
import Data.Functor
import qualified System.Console.Terminal.Size as Terminal

import Paths_thrill

data Build = Build String
data Format = Format String
data Infer = Infer String
data Desugar = Desugar String String
data Run = Run String
data Core = Core String (Maybe String) Bool
data Codegen = Codegen String Bool
data Compile = Compile
  { compileFile :: String
  , compileOutputFile :: Maybe String
  , compileEmitLlvm :: Bool
  }

fileArg = strArgument (metavar "FILE" <> help "location of source file")
stageArg = strArgument (metavar "STAGE")
filterArg = optional . strOption $ long "filter" <> metavar "FILTER" <> short 'f'
  <> help "only print the binding that exactly matches the filter provided"
outputFileArg = optional . strOption $ long "output-file" <> metavar "FILTER" <> short 'o'
  <> help "provide  a name for the final executable"

globalFlags = flag True False (long "no-default-prelude" <> help "Disable the implicit prelude module")

options = do
  simpleOptions "v0.1.0" "thrill: lol" "omg" (globalFlags) $ do
    addSubCommands "debug" "debug commands" $ do
      addCommand "infer"
        "run the typechecker and output information about methods and traits"
        inferC (Infer <$> fileArg)
      addCommand "run"
        "execute the main function in a module using a lazy interpreter"
        run    (Run <$> fileArg)
      addCommand "core"
        "generate core bindings for a module"
        core   (Core <$> fileArg <*> filterArg <*> (flag False True $ long "only-lint"))
      addCommand "desugar"
        "view the desugaring pipeline up to a given stage"
        desugarC (Desugar <$> stageArg <*> fileArg)
      addCommand "codegen"
        "run the code generator and prettyprint llvm ir"
        codegenC (Codegen <$> fileArg <*> (flag False True $ long "print-ir"))

    addCommand "format"
      "prettyprint the module at a given location"
      format (Format <$> fileArg)
    addCommand "compile"
      "compile a module into an executable binary"
      compileC (Compile <$> fileArg <*> outputFileArg <*> (flag False True $ long "emit-llvm"))

codegenC (Codegen f toPrint)            = commandWrapper f (codegen toPrint)
inferC   (Infer f)                      = commandWrapper f (infer)
run      (Run f)                        = commandWrapper f (runInterpreter)
core     (Core f filter lint)           = commandWrapper f (coreDebug filter lint)
desugarC (Desugar s f)                  = commandWrapper f (desugar s)
compileC (Compile file oFile emitLlvm)  = commandWrapper file (compile oFile emitLlvm)
format   (Format file) gOpts _          = do
  stream <- T.readFile file
  let parsed = runParser thrillParser (file) stream

  case parsed of
    Left err -> putStrLn $ parseErrorPretty' stream err
    Right mod -> T.putStrLn . renderThrill (renderArgs gOpts) $ pretty mod

commandWrapper :: FilePath
  -> (GlobalOptions -> RenamedModule SourceSpan -> IO ())
  -> GlobalOptions
  -> Either (Parser.ParseError Char Void) (Module String SourceSpan)
  -> IO ()
commandWrapper file com gOpts parsedPrelude = do
  stream <- T.readFile file
  let parsed = runParser thrillParser (file) stream
  let joined = (,) <$> parsedPrelude <*> parsed
  case joined of
    Left err -> putStrLn $ parseErrorPretty' stream err
    Right (prelude, ast) -> do
      let
        mod' = bindingGroups prelude >>= runRenamer >>= \(prelude', state) -> do
          (ast', _) <- bindingGroups ast >>= renameModule' state
          return $ (prelude' <&> (<>)) `moduleApply` ast'

      case mod' of
        Left err -> T.putStrLn $ renderThrill (renderArgs gOpts) (prettyError err)
        Right m -> com gOpts m

main :: IO ()
main = do
  (opts, cmd) <- options

  term <- Terminal.size
  let renderArgs = case term of
        Just term -> defaultRenderArgs { width = Terminal.width term }
        Nothing -> defaultRenderArgs
      gOpts = GlobalOpts renderArgs

  parsedPrelude <- case opts of
    True -> do
      preludePath <- getDataFileName "assets/prelude.ill"
      parseFromFile moduleParser preludePath
    False -> pure $ pure (Module "Prelude" [])
  cmd gOpts parsedPrelude

mergeModules (Module _ ds) (Module n ds2) = Module n (ds ++ ds2)
