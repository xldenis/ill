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
import Ill.Desugar.Cases

import Control.Monad.State (runStateT)
import Control.Monad.Except (runExcept)

import Ill.Syntax.Pretty
import Ill.BindingGroup
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

      let Right (typed, _) = runTC mod
          ValueBG [x] = last typed
          result = simplifyPatterns x
          expected = [decl|
            fn mappairs()
              fn (x1, x2, x3) =
                f = x1
                case x2 of
                  when C x xs: case x3 of
                    when C y ys: C(f(x, y), mappairs(f, xs, ys))
                    when Nil: Nil
                  end
                  when Nil: ys = x3
                    Nil
                end
              end
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
          matcher = runFresh $ match ["u"] (declToEqns x)
          result  = matcher (undefined :< Var "zzzx")
          expected = [expr|
            case u of
              when C a as: 1
              when Nil: 2
            end
          |]

          in dropAnn result `shouldBe` dropAnn expected

