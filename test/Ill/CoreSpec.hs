{-# LANGUAGE OverloadedStrings #-}
module Ill.CoreSpec where

-- import Ill.Quote
import Text.Megaparsec (runParser)

import Test.Hspec

import SpecHelper

import Ill.Parser

import Control.Monad.IO.Class

import Ill.Syntax
import Ill.Infer.Monad
import Ill.Infer
import Ill.Syntax.Core

import Control.Monad.State (runStateT)
import Control.Monad.Except (runExcept)

import Ill.Syntax.Pretty
import Ill.Desugar
import Data.Text.Lazy.IO as T

runTC (Module _ ds) = unCheck (bindingGroups ds >>= typeCheck)

unCheck c = runExcept $ runStateT (runCheck c) defaultCheckEnv


spec :: Spec
spec = do
  describe "" $ do
    it "" $ do
      Right mod <- liftIO $ parseFromFile illParser "test/core/core_1.ill"
      case runTC mod of
        Left err -> expectationFailure "omg"
        Right (typed, _) -> do
          let ValueBG [x] = last typed
              matcher = match ["u"] tNil (declToEqns x)
              result  = matcher (undefined :< Var "zzzx")
          liftIO . T.putStrLn . renderIll' $ pretty result
          True `shouldBe` True