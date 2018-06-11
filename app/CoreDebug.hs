module CoreDebug where

import Control.Arrow
import Control.Monad

import Data.Bifunctor (first, bimap)
import Data.Function ((&))
import Data.Text.Lazy hiding (map)
import Data.Text.Lazy.IO

import Ill.Options
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

coreDebug :: Maybe String -> Bool -> GlobalOptions -> Module SourceSpan -> IO ()
coreDebug filter onlyLint gOpts ast = case typeCheckModule ast of
  Left err -> putStrLn . renderError gOpts $ prettyError err
  Right (mod, env) -> do
    let desugared = defaultPipeline env mod
        core = compileCore desugared
        binds = filterBindings filter (bindings core)

    unless onlyLint $ do
      putStrLn $ pack "\n\nCORE OUTPUT\n\n"
      putStrLn $ renderError gOpts (vcat $ map pretty $ binds)

    case runLinter core of
      Left err -> putStrLn $ pack err
      Right () -> do
        putStrLn $ pack "omgyesss: passed core lint!"

  where
  cliRenderArgs = defaultRenderArgs { width = 90 }

renderError opts = renderIll (renderArgs opts)

filterBindings :: Maybe String -> [Bind Var] -> [Bind Var]
filterBindings Nothing binds = binds
filterBindings (Just f) binds = maybeToList $ L.find finder binds
  where finder (NonRec v _) = varName v == f
