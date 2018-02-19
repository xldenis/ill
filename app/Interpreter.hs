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

import Ill.Syntax hiding (Expression(..))
import Ill.Syntax.Core as Core
import Ill.Syntax.Pretty

import Prelude hiding (putStrLn, putStr)

import qualified Ill.Interpret as Interp

runInterpreter mod = do
  case  compileToCore mod of
    Left err -> putStrLn $ err
    Right (coreMod) -> do
      let boundConstructors = map (\(c, (arity, _, _)) -> (c, arity)) $ Core.constructors coreMod

      env <- Interp.mkEnvForModule boundConstructors (bindings coreMod)
      val <- Interp.eval env (Var $ Id "main" undefined Used)

      print (Interp.showish val)

runTC (Module _ ds) = unCheck (bindingGroups ds >>= typeCheck) >>= pure . bimap fromBindingGroups env

unCheck c = execCheck c

prettyType a = renderIll defaultRenderArgs (pretty $ a)

compileToCore mod =  do
  case runTC mod of
    Right (typed, e) -> let
      desugared = defaultPipeline e typed
      in Right (compileCore desugared)
    Left err -> Left $ prettyType err
