module Interpreter where

import Ill.BindingGroup

import Ill.Desugar

import Ill.Infer
import Ill.Infer.Monad

import Ill.Interpret

import Ill.Syntax
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

runInterpreter mod = do
  parsedPrelude <- parseFromFile moduleParser "assets/prelude.ill"

  prelude <- case parsedPrelude of
    Right prelude -> return prelude
    Left err -> error $ "prelude failed to parse: " ++ parseErrorPretty err

  let mod' = mergeModules prelude mod
  (coreMod, moduleCons) <- compileToCore mod'

  let boundConstructors = moduleCons
      getBinding (NonRec n e) = (name n, e)
      boundNames = map getBinding coreMod
      context = Context boundNames boundConstructors []

  let mainExpr = case find (\(NonRec n _) -> (name n) == "main") coreMod of
                  Just (NonRec _ mainExpr) -> mainExpr
                  Nothing -> error "no main function is defined!"

  case runExcept . (flip evalStateT context) $ interpret mainExpr of
    Right result -> putStrLn $ renderIll' (pretty result)
    Left  error  -> putStrLn . pack $ "intrepretation error: " ++ (show $ pretty error)

runTC (Module _ ds) = unCheck (bindingGroups ds >>= typeCheck) >>= pure . bimap fromBindingGroups env

unCheck c = execCheck c

prettyType a = renderIll defaultRenderArgs (pretty $ a)

mergeModules (Module _ ds) (Module n ds2) = Module n (ds ++ ds2)

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
      in return (declToCore desugared, desugared >>= getConstructorArities)
    Left err -> error . show $ prettyType err
