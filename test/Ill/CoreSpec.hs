{-# LANGUAGE QuasiQuotes #-}
module Ill.CoreSpec where

import Ill.Quote
import Text.Megaparsec (runParser)

import Test.Hspec

import SpecHelper

import Control.Monad.State (runStateT)
import Control.Monad.Except (runExcept)
import Control.Monad.IO.Class

import Ill.Parser
import Ill.Syntax
import Ill.Syntax.Core as C hiding (Var)
import qualified Ill.Syntax.Core as C
import Ill.Infer
import Ill.Desugar.Cases
import Ill.Syntax.Pretty
import Ill.BindingGroup

import Data.Text.Lazy.IO as T
import Data.Maybe

runTC (Module _ ds) = unCheck (bindingGroups ds >>= typeCheck)

mkVar nm = Id { varName = nm, C.ty = tNil, usage = Used }
spec :: Spec
spec = do
  describe "substitution" $ do
    describe "in nested bindings" $ do
      it "respects let statements" $ do
        let orig = Let (NonRec (mkVar "a") (Lit $ Integer 1)) (C.Var "b")
        substitute ("a", C.Var "c") orig `shouldBe` orig
      it "respects lambdas" $ do
        let orig = C.Lambda (mkVar "a") (C.Var "b")
        substitute ("a", C.Var "c") orig `shouldBe` orig

    it "substitutes free variables in binders" $ do
      let orig = Let (NonRec (mkVar "a") (C.Var $ "free")) (C.Var "b")
      let subd = Let (NonRec (mkVar "a") (C.Var $ "c"))    (C.Var "b")
      substitute ("free", C.Var "c") orig `shouldBe` subd
    it "substitutes free vars in lambda" $ do
      let orig = C.Lambda (mkVar "a") (C.Var "b")
      substitute ("b", C.Var "c") orig `shouldNotBe` orig
    it "substitutes free vars in let" $ do
      let orig = Let (NonRec (mkVar "a") (Lit $ Integer 1)) (C.Var "b")
      substitute ("b", C.Var "c") orig `shouldNotBe` orig
    it "substitutes free vars in case" $ do
      let orig = C.Case (C.Var "bound") [TrivialAlt $ C.Var "free" :: Alt C.Var]
      let subd = C.Case (C.Var "bound") [TrivialAlt $ C.Var "changed" :: Alt C.Var]
      substitute ("free", C.Var "changed") orig `shouldBe` subd

      let orig = C.Case (C.Var "bound") [TrivialAlt $ C.Var "bound2", ConAlt "con" [mkVar "free"] (C.Var "free")]
      let subd = C.Case (C.Var "bound") [TrivialAlt $ C.Var "bound2", ConAlt "con" [mkVar "free"] (C.Var "free")]
      substitute ("free", C.Var "changed") orig `shouldBe` subd

      let orig = C.Case (C.Var "bound") [TrivialAlt $ C.Var "bound2", ConAlt "con" [mkVar "binder"] (C.Var "free")]
      let subd = C.Case (C.Var "bound") [TrivialAlt $ C.Var "bound2", ConAlt "con" [mkVar "binder"] (C.Var "changed")]
      substitute ("free", C.Var "changed") orig `shouldBe` subd


  describe "desugaring" $ do
    it "desugars constructor groups" $ do
      let mod = [modQ|
        module X
          data L a = C a (L a) | Nil

          fn mappairs(x)
            case x of
              when C el Nil: 1
              when C el ls:  2
            end
          end
        end
      |]

      let Right (typed, _) = runTC mod
          ValueBG [x] = last typed
          result = simplifyPatterns x
          expected = [decl|
            fn mappairs()
              fn (x1) =
                case x1 of
                  when C el omg0: case omg0 of
                    when Nil: 1
                    when b  : 2
                  end
                end
              end
            end
          |]

      renderIll' (pretty result) `shouldBe` renderIll' (pretty expected)

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

