module DesugarDebug where

import Ill.Syntax
import Ill.Infer
import Ill.Infer.Monad

import Data.Function

import Control.Monad.State (runStateT)
import Control.Monad.Except (runExcept)
import Control.Monad

import Ill.Desugar
import Ill.Desugar.Trait
import Ill.Desugar.Cases

import Prelude hiding (putStrLn, putStr)
import Data.Text.Lazy.IO
import Data.Text.Lazy hiding (map)
import Ill.Syntax.Pretty

import Data.Bifunctor (first, bimap)

desugar :: String -> Module SourceSpan -> IO ()
desugar stage ast = case runTC ast of
  Left err -> putStrLn $ prettyType err
  Right (typed, env) -> do
    let pipeline = stageToPipeline stage
        desugared = pipeline env typed

    print (traitDictionaries env)
    putStrLn $ renderIll defaultRenderArgs (pretty $ Module "t" desugared)

runTC (Module _ ds) = unCheck (bindingGroups ds >>= typeCheck) >>= pure . bimap fromBindingGroups env

unCheck c = runExcept $ runStateT (runCheck c) defaultCheckEnv

stageToPipeline :: String -> (Environment -> [Decl TypedAnn] -> [Decl TypedAnn])
stageToPipeline "traits" e = desugarTraits e
stageToPipeline "cases"  e = desugarTraits e >=> pure . simplifyPatterns
stageToPipeline _ _ = id

prettyType a = renderIll defaultRenderArgs (pretty $ a)
