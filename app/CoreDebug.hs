module CoreDebug where

import Ill.Syntax
import Ill.Syntax.Core
import Ill.Infer
import Ill.Infer.Monad

import Ill.BindingGroup
import Ill.Desugar

import Ill.CoreLint

import Control.Arrow

import Ill.Syntax.Pretty

import Control.Monad

import Prelude hiding (putStrLn, putStr)
import Data.Text.Lazy.IO
import Data.Text.Lazy hiding (map)
import Data.Bifunctor (first, bimap)

coreDebug :: Module SourceSpan -> IO ()
coreDebug ast = case runTC ast of
  Left err -> putStrLn $ prettyType err
  Right (typed, env) -> do
    let desugared = pipeline env typed
        core = declsToCore desugared

    putStrLn $ pack "\n\nCORE OUTPUT\n\n"
    putStrLn $ renderIll cliRenderArgs (vcat $ map pretty $ bindings $ core)

    case runLinter core of
      Left err -> putStrLn $ pack err
      Right () -> do
        putStrLn $ pack "omgyesss"

  where
  cliRenderArgs = defaultRenderArgs { width = 50 }
  pipeline e = desugarBinOps >>> desugarTraits e >=> pure . simplifyPatterns

runTC (Module _ ds) = execCheck (bindingGroups ds >>= typeCheck) >>= pure . bimap fromBindingGroups env
prettyType a = renderIll defaultRenderArgs (pretty $ a)
