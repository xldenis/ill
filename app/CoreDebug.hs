module CoreDebug where

import Control.Arrow
import Control.Monad

import Data.Bifunctor (first, bimap)
import Data.Function ((&))
import Data.Text.Lazy hiding (map)
import Data.Text.Lazy.IO

import Ill.BindingGroup
import Ill.CoreLint
import Ill.Desugar

import Ill.Infer
import Ill.Infer.Monad

import Ill.Syntax
import Ill.Syntax.Core
import Ill.Syntax.Pretty

import Prelude hiding (putStrLn, putStr)

coreDebug :: Module SourceSpan -> IO ()
coreDebug ast = case runTC ast of
  Left err -> putStrLn $ prettyType err
  Right (typed, env) -> do
    let desugared = pipeline env typed
        core = declsToCore desugared & normalize

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
