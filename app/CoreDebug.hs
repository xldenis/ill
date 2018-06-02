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

import Data.List as L (find)
import Data.Maybe (maybeToList)

import Prelude hiding (putStrLn, putStr)

coreDebug :: Maybe String -> Bool -> Module SourceSpan -> IO ()
coreDebug filter onlyLint ast = case runTC ast of
  Left err -> putStrLn $ prettyType err
  Right (typed, env) -> do
    let desugared = defaultPipeline env typed
        core = compileCore desugared
        binds = filterBindings filter (allBinds core)

    unless onlyLint $ do
      putStrLn $ pack "\n\nCORE OUTPUT\n\n"
      putStrLn $ renderIll cliRenderArgs (vcat . map pretty $ binds)

    case runLinter core of
      Left err -> putStrLn $ pack err
      Right () -> do
        putStrLn $ pack "omgyesss: passed core lint!"

  where
  cliRenderArgs = defaultRenderArgs { width = 90 }

runTC (Module _ ds) = execCheck (bindingGroups ds >>= typeCheck) >>= pure . bimap fromBindingGroups env
prettyType a = renderIll defaultRenderArgs (pretty $ a)

filterBindings :: Maybe String -> [Bind Var] -> [Bind Var]
filterBindings Nothing binds = binds
filterBindings (Just f) binds = maybeToList $ L.find finder binds
  where finder (NonRec v _) = varName v == f
