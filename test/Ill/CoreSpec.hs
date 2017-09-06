{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Ill.CoreSpec where

import Ill.Quote
import Text.Megaparsec (runParser)

import Test.Hspec

import SpecHelper

import Ill.Parser

import Control.Monad.IO.Class

import Ill.Syntax
-- import Ill.Infer.Monad
import Ill.Infer
import Ill.Syntax.Core

import Control.Monad.State (runStateT)
import Control.Monad.Except (runExcept)

import Ill.Syntax.Pretty
import Ill.Desugar
import Data.Text.Lazy.IO as T
import Data.Maybe

runTC (Module _ ds) = unCheck (bindingGroups ds >>= typeCheck)

-- unCheck c = runExcept $ runStateT (runCheck c) defaultCheckEnv


spec :: Spec
spec = do
  describe "" $ do
    it "mappairs" $ do
      let mod = [modQ|
        module X
          data L a = C a (L a) | Nil

          fn mappairs(f, Nil, ys)
            Nil
          or mappairs(f, C x xs, Nil)
            Nil
          or mappairs(f, C x xs, C y ys)
            C(f(x, y), mappairs(f, xs, ys))
          end
        end
      |]

      let result = match ["u", "v", "w"] (declToEqns . fromJust $ lookupFn "mappairs" mod) (undefined :< Var "z")
          expected = [expr|
            case v of
              when C x xs: case w of
                when C y ys: C(f(x, y), mappairs(f, xs, ys))
                when Nil: Nil
              end
              when Nil: Nil
            end
          |]


      renderIll' (pretty result) `shouldBe` renderIll' (pretty expected)

    it "" $ do

      let mod = [modQ|
        module X
          data L a = C a (L a) | Nil
          fn a (C a as)
            1
          or a (Nil)
            2
          end
        end
      |]

      case runTC mod of
        Right (typed, _) -> let
          ValueBG [x] = last typed
          matcher = match ["u"] (declToEqns x)
          result  = matcher (undefined :< Var "zzzx")
          expected = [expr|
            case u of
              when C a as: 1
              when Nil: 2
            end
          |]

          in (fmap (const ()) result) `shouldBe` (fmap (const ()) expected)

