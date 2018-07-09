module Interpreter where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State

import Data.Bifunctor
import Data.List
import Data.Maybe
import Data.Text.Lazy (pack)
import Data.Text.Lazy.IO

import Thrill.BindingGroup
import Thrill.Desugar as Desugar

import Thrill.Infer
import Thrill.Infer.Monad

import Thrill.Interpret
import Thrill.Parser
import Thrill.Renamer

import Thrill.Syntax hiding (Expression(..))
import Thrill.Syntax.Core as Core
import Thrill.Syntax.Pretty

import Data.String (fromString)

import Prelude hiding (putStrLn, putStr)

import qualified Thrill.Interpret as Interp

runInterpreter _ mod = do
  case compileToCore mod of
    Left err -> putStrLn $ renderError (prettyError err)
    Right (coreMod) -> do
      let boundConstructors = map (fmap consArity) $ Core.constructors coreMod

      env <- Interp.mkEnvForModule boundConstructors (bindings coreMod)
      val <- Interp.eval env (Var $ Id (Qualified (moduleName mod) "main") undefined Used)

      print (Interp.showish val)

renderError = renderThrill defaultRenderArgs

compileToCore mod = do
  case (typeCheckModule) mod of
    Right (typed, e) -> let
      desugared = defaultPipeline e typed
      in Right (compileCore desugared)
    Left err -> Left err
