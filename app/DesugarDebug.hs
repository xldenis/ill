module DesugarDebug where

import Thrill.Syntax
import Thrill.Syntax.Core (bindings)
import Thrill.Infer
import Thrill.Infer.Monad
import Thrill.Options
import Thrill.Renamer

import Data.Function

import Control.Monad.State (runStateT)
import Control.Monad.Except (runExcept)
import Control.Monad

import Thrill.BindingGroup
import Thrill.Desugar

import Prelude hiding (putStrLn, putStr)
import Data.Text.Lazy.IO
import Data.Text.Lazy hiding (map)
import Thrill.Syntax.Pretty

import Data.Bifunctor (first, bimap)

desugar :: String -> GlobalOptions -> RenamedModule SourceSpan -> IO ()
desugar stage gOpts ast = case (typeCheckModule) ast of
  Left err -> putStrLn . render gOpts $ prettyError err
  Right (mod, env) -> do
    let pipeline = stageToPipeline stage
        desugared = pipeline env mod

    putStrLn $ render gOpts (pretty desugared)
  where
  cliRenderArgs = defaultRenderArgs { width = 50}

stageToPipeline :: String -> (Environment -> Module QualifiedName TypedAnn -> Module QualifiedName TypedAnn)
stageToPipeline "binop"  e = desugarBinOps
stageToPipeline "traits" e = desugarTraits e . stageToPipeline "binop" e
stageToPipeline "cases"  e = desugarPatterns . stageToPipeline "traits" e
stageToPipeline _ _ = id

render :: GlobalOptions -> Doc AnsiStyle -> Text
render opts = renderThrill (renderArgs opts)
