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
import Ill.Syntax.Core
import Ill.Syntax.Pretty

import Prelude hiding (putStrLn, putStr)

import qualified Ill.Interpret as Interp

runInterpreter mod = do
  case  compileToCore mod of
    Left err -> putStrLn $ err
    Right (coreMod) -> do
      let boundConstructors = Desugar.constructors coreMod

      env <- Interp.mkEnvForModule boundConstructors (bindings coreMod)
      val <- Interp.eval env (Var "main")

      print (Interp.showish val)

runTC (Module _ ds) = unCheck (bindingGroups ds >>= typeCheck) >>= pure . bimap fromBindingGroups env

unCheck c = execCheck c

prettyType a = renderIll defaultRenderArgs (pretty $ a)

desugaringPipeline :: Environment -> [Decl TypedAnn] -> [Decl TypedAnn]
desugaringPipeline env = desugarTraits env . desugarBinOps >=> pure . simplifyPatterns

getConstructorArities (_ :< Data nm _ conses) = map (\cons ->
  case unwrapProduct cons of
    (TConstructor consNm : args) -> (consNm, length args)
  ) conses
getConstructorArities _ = []

compileToCore mod =  do
  case runTC mod of
    Right (typed, e) -> let
      desugared = desugaringPipeline e typed
      in Right (declsToCore desugared)
    Left err -> Left $ prettyType err
