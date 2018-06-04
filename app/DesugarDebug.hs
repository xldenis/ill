module DesugarDebug where

import Ill.Syntax
import Ill.Syntax.Core (bindings)
import Ill.Infer
import Ill.Infer.Monad

import Data.Function

import Control.Monad.State (runStateT)
import Control.Monad.Except (runExcept)
import Control.Monad

import Ill.BindingGroup
import Ill.Desugar

import Prelude hiding (putStrLn, putStr)
import Data.Text.Lazy.IO
import Data.Text.Lazy hiding (map)
import Ill.Syntax.Pretty

import Data.Bifunctor (first, bimap)

desugar :: String -> Module SourceSpan -> IO ()
desugar stage ast = case typeCheckModule ast of
  Left err -> putStrLn $ prettyType err
  Right (mod, env) -> do
    let pipeline = stageToPipeline stage
        desugared = pipeline env mod

    -- print (map pretty $ traitDictionaries env)
    putStrLn $ renderIll' (pretty desugared)
  where
  cliRenderArgs = defaultRenderArgs { width = 50}

stageToPipeline :: String -> (Environment -> Module TypedAnn -> Module TypedAnn)
stageToPipeline "binop"  e = desugarBinOps
stageToPipeline "traits" e = desugarTraits e . stageToPipeline "binop" e
stageToPipeline "cases"  e = desugarPatterns . stageToPipeline "traits" e
stageToPipeline _ _ = id

prettyType a = renderIll defaultRenderArgs (pretty $ a)
