module Interpreter where

import Ill.BindingGroup

import Ill.Desugar

import Ill.Infer
import Ill.Infer.Monad

import Ill.Interpret

import Ill.Syntax hiding (Expression(..))
import Ill.Syntax.Core
import Ill.Syntax.Pretty
import Ill.Parser

import Control.Monad.State
import Control.Monad.Except
import Control.Monad

import Data.Bifunctor
import Data.Maybe
import Data.List

import Prelude hiding (putStrLn, putStr)
import Data.Text.Lazy.IO
import Data.Text.Lazy (pack)

import qualified Ill.Interpret as Interp

runInterpreter mod = do
  case  compileToCore mod of
    Left err -> putStrLn $ err
    Right (coreMod, moduleCons) -> do
      let boundConstructors = moduleCons

      env <- Interp.mkEnvForModule boundConstructors coreMod
      val <- Interp.eval env (Var "main")

      print (Interp.showish val)

runTC (Module _ ds) = unCheck (bindingGroups ds >>= typeCheck) >>= pure . bimap fromBindingGroups env

unCheck c = execCheck c

prettyType a = renderIll defaultRenderArgs (pretty $ a)

desugaringPipeline env = (desugarTraits env . desugarBinOps) >=> pure . simplifyPatterns

getConstructorArities (_ :< Data nm _ conses) = map (\cons ->
  case unwrapProduct cons of
    (TConstructor consNm : args) -> (consNm, length args)
  ) conses
getConstructorArities _ = []

compileToCore mod =  do
  case runTC mod of
    Right (typed, e) -> let
      desugared = desugaringPipeline e typed
      in Right (declToCore desugared, desugared >>= getConstructorArities)
    Left err -> Left $ prettyType err
