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

desugar :: String -> Module SourceSpan -> IO ()
desugar stage ast = case runTC ast of
  Left err -> putStrLn $ prettyType err
  Right typed -> do
    let pipeline = stageToPipeline stage
        desugared = pipeline typed

    putStrLn $ renderIll defaultRenderArgs (pretty $ Module "t" desugared)

runTC (Module _ ds) = unCheck (bindingGroups ds >>= typeCheck) >>= pure . fromBindingGroups . fst

unCheck c = runExcept $ runStateT (runCheck c) defaultCheckEnv

stageToPipeline :: String -> ([Decl TypedAnn] -> [Decl TypedAnn])
stageToPipeline "traits" = desugarTraits
stageToPipeline "cases"  = desugarTraits >=> pure . simplifyPatterns
stageToPipeline _ = id

prettyType a = renderIll defaultRenderArgs (pretty $ a)
