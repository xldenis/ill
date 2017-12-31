{-# LANGUAGE QuasiQuotes #-}
module Ill.CoreSpec where

import Ill.Quote
import Text.Megaparsec (runParser)

import Test.Hspec

import SpecHelper

import Control.Monad.IO.Class

import Ill.Parser
import Ill.Syntax
import Ill.Syntax.Core as C hiding (Var)
import qualified Ill.Syntax.Core as C
import Ill.Infer
import Ill.Infer.Monad (execCheck)
import Ill.Desugar
import Ill.Syntax.Pretty
import Ill.BindingGroup
import Ill.CoreLint

import Data.Text.Lazy.IO as T
import Data.Maybe
import Data.Bifunctor
-- import Data.Function

import Control.Arrow
import Control.Monad

import System.Directory
import System.FilePath

runTC (Module _ ds) = unCheck (bindingGroups ds >>= typeCheck)

mkVar nm = Id { varName = nm, C.idTy = tNil, usage = Used }

moduleToCore :: Environment -> [Decl TypedAnn] -> CoreModule
moduleToCore e = (desugarBinOps >>> desugarTraits e >=> pure . simplifyPatterns) >>> declsToCore >>> normalize >>> liftModule

runTC' (Module _ ds) = execCheck (bindingGroups ds >>= typeCheck) >>= pure . bimap fromBindingGroups env

coreLintSpec path = do
  parsed <- parseFromFile moduleParser path
  case parsed of
    Left e -> expectationFailure $
      "the parser is expected to succeed, but it failed with:\n" ++
      showParseError e
    Right ast -> case runTC' ast of
      Left err -> expectationFailure . show $ pretty err
      Right (typed, env) -> do
        case runLinter (moduleToCore env typed) of
          Left err -> expectationFailure err
          Right () -> return ()

-- filesShouldFail :: Show b => FilePath -> Parsec Void Text b -> Spec
lintCoreInDir dir = do
  fs <- runIO $ getFilesInDir dir

  describe ("the files in " ++ dir ++ " pass core linter") $ do
    forM_ fs $ \f -> do
      it (takeFileName f ++ " succeeds.") $ do
        coreLintSpec f


spec :: Spec
spec = do
  lintCoreInDir "test/core"
  describe "substitution" $ do
    describe "in nested bindings" $ do
      it "respects let statements" $ do
        let orig = Let (NonRec (mkVar "a") (Lit $ Integer 1)) (C.Var $ mkVar "b")
        substitute ("a", C.Var $ mkVar "c") orig `shouldBe` orig
      it "respects lambdas" $ do
        let orig = C.Lambda (mkVar "a") (C.Var $ mkVar "b")
        substitute ("a", C.Var $ mkVar "c") orig `shouldBe` orig

    it "substitutes free variables in binders" $ do
      let orig = Let (NonRec (mkVar "a") (C.Var $ mkVar "free")) (C.Var $ mkVar "b")
      let subd = Let (NonRec (mkVar "a") (C.Var $ mkVar "c"))    (C.Var $ mkVar "b")
      substitute ("free", C.Var $ mkVar "c") orig `shouldBe` subd
    it "substitutes free vars in lambda" $ do
      let orig = C.Lambda (mkVar "a") (C.Var $ mkVar "b")
      substitute ("b", C.Var $ mkVar "c") orig `shouldNotBe` orig
    it "substitutes free vars in let" $ do
      let orig = Let (NonRec (mkVar "a") (Lit $ Integer 1)) (C.Var $ mkVar "b")
      substitute ("b", C.Var $ mkVar "c") orig `shouldNotBe` orig
    it "substitutes free vars in case" $ do
      let boundVar   = C.Var $ Id "bound" (TVar "a") Used
          boundVar2  = C.Var $ Id "bound2" (TVar "a") Used
          freeVar    = C.Var $ Id "free" (TVar "a") Used
          changedVar = C.Var $ Id "changed" (TVar "a") Used

      let orig = C.Case boundVar [TrivialAlt $ freeVar :: Alt C.Var]
      let subd = C.Case boundVar [TrivialAlt $ changedVar :: Alt C.Var]
      substitute ("free", changedVar) orig `shouldBe` subd

      let orig = C.Case boundVar [TrivialAlt $ boundVar2, ConAlt "con" [mkVar "free"] (freeVar)]
      let subd = C.Case boundVar [TrivialAlt $ boundVar2, ConAlt "con" [mkVar "free"] (freeVar)]
      substitute ("free", changedVar) orig `shouldBe` subd

      let orig = C.Case boundVar [TrivialAlt $ boundVar2, ConAlt "con" [mkVar "binder"] (freeVar)]
      let subd = C.Case boundVar [TrivialAlt $ boundVar2, ConAlt "con" [mkVar "binder"] (changedVar)]
      substitute ("free", changedVar) orig `shouldBe` subd


  describe "desugaring" $ do
    it "desugars constructor groups" $ do
      constructorGroups
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
                    when _ : failedPattern
                  end
                  when Nil: ys = x3
                    Nil
                  when _ : failedPattern
                end
              end
            end
          |]

      renderIll' (pretty result) `shouldBe` renderIll' (pretty expected)

    it "simple module" $ do

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
              when _ : zzzx
            end
          |]

          in renderIll' (pretty result) `shouldBe` renderIll' (pretty expected)

constructorGroups = do
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
            x = x1
            case x of
              when C el omg0: case omg0 of
                when Nil: 1
                when _  : ls = omg0
                2
              end
              when _ : failedPattern
            end
          end
        end
      |]

  renderIll' (pretty result) `shouldBe` renderIll' (pretty expected)

