module Interpreter where

import Ill.Syntax
import Ill.Infer
import Ill.Infer.Monad
import Ill.Desugar
import Ill.Interpret
import Ill.Syntax.Core
import Ill.Syntax.Pretty
import Ill.BindingGroup

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
  case runTC mod of
    Left err -> putStrLn $ prettyType err
    Right (typed, e) -> do
      let desugared = desugaringPipeline e typed
          core = declToCore desugared
          boundConstructors = desugared >>= getConstructorArities
          context = Context (map (\(NonRec n e) -> (name n, e)) core) boundConstructors []
          mainExpr = case find (\(NonRec n _) -> (name n) == "main") core of
            Just (NonRec _ mainExpr) -> mainExpr
            Nothing -> error "no main function is defined!"
      case runExcept . (flip evalStateT context) $ interpret mainExpr of
        Right result -> putStrLn $ renderIll' (pretty result)
        Left  error  -> putStrLn . pack$ "intrepretation error: " ++ error

runTC (Module _ ds) = unCheck (bindingGroups ds >>= typeCheck) >>= pure . bimap fromBindingGroups env

unCheck c = execCheck c

prettyType a = renderIll defaultRenderArgs (pretty $ a)

desugaringPipeline env = (desugarTraits env . desugarBinOps) >=> pure . simplifyPatterns
getConstructorArities (_ :< Data nm _ conses) = map (\cons ->
  case unwrapProduct cons of
    (TConstructor consNm : args) -> (consNm, length args)
  ) conses
getConstructorArities _ = []
