module DesugarDebug where

import Ill.Syntax
import Ill.Infer
import Ill.Infer.Monad

import Data.Function

import Control.Monad.State (runStateT)
import Control.Monad.Except (runExcept)
import Control.Monad

import Ill.BindingGroup
import Ill.Desugar.Trait
import Ill.Desugar.Cases
import Ill.Desugar

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

    print (map pretty $ traitDictionaries env)
    putStrLn $ renderIll' (pretty $ Module "t" desugared)
    putStrLn $ pack "\n\nCORE OUTPUT\n\n"
    putStrLn $ renderIll cliRenderArgs (vcat $ map pretty $ declToCore desugared)
  where
  cliRenderArgs = defaultRenderArgs { width = 50}

runTC (Module _ ds) = unCheck (bindingGroups ds >>= typeCheck) >>= pure . bimap fromBindingGroups env

unCheck c = runExcept $ runStateT (runCheck c) defaultCheckEnv

stageToPipeline :: String -> (Environment -> [Decl TypedAnn] -> [Decl TypedAnn])
stageToPipeline "traits" e = desugarTraits e
stageToPipeline "cases"  e = desugarTraits e >=> pure . simplifyPatterns
stageToPipeline _ _ = id

prettyType a = renderIll defaultRenderArgs (pretty $ a)
