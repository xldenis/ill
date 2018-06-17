module Interpreter where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State

import Data.Bifunctor
import Data.List
import Data.Maybe
import Data.Text.Lazy (pack)
import Data.Text.Lazy.IO

import Ill.BindingGroup
import Ill.Desugar as Desugar

import Ill.Infer
import Ill.Infer.Monad

import Ill.Interpret
import Ill.Parser
import Ill.Renamer

import Ill.Syntax hiding (Expression(..))
import Ill.Syntax.Core as Core
import Ill.Syntax.Pretty

import Data.String (fromString)

import Prelude hiding (putStrLn, putStr)

import qualified Ill.Interpret as Interp

runInterpreter _ mod = do
  case compileToCore mod of
    Left err -> putStrLn $ renderError (prettyError err)
    Right (coreMod) -> do
      let boundConstructors = map (fmap consArity) $ Core.constructors coreMod

      env <- Interp.mkEnvForModule boundConstructors (bindings coreMod)
      val <- Interp.eval env (Var $ Id (Qualified (moduleName mod) "main") undefined Used)

      print (Interp.showish val)

renderError = renderIll defaultRenderArgs

compileToCore mod = do
  case (execTypecheckModule) mod of
    Right (typed, e) -> let
      desugared = defaultPipeline e typed
      in Right (compileCore desugared)
    Left err -> Left err
