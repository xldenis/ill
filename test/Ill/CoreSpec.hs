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
import Ill.Renamer
import Ill.Syntax.Pretty
import Ill.BindingGroup
import Ill.CoreLint
import Ill.Error (prettyError)

import Data.Text.Lazy.IO as T
import Data.Maybe
import Data.Bifunctor

import Control.Arrow
import Control.Monad

import System.Directory
import System.FilePath

runTC mod = bindingGroups mod >>= renameModule >>= typeCheckModule

mkVar nm = Id { varName = Internal nm, C.idTy = tNil, usage = Used }

moduleToCore :: Environment -> Module QualifiedName TypedAnn -> CoreModule
moduleToCore e = (defaultPipeline e) >>> compileCore

runTC' m = bindingGroups m >>= renameModule >>= typeCheckModule

coreLintSpec path = do
  parsed <- parseFromFile moduleParser path
  case parsed of
    Left e -> expectationFailure $
      "the parser is expected to succeed, but it failed with:\n" ++
      showParseError e
    Right ast -> case runTC' (preludifyModule ast) of
      Left err -> expectationFailure . show $ prettyError err
      Right (typed, env) -> do
        case runLinter (moduleToCore env typed) of
          Left err -> expectationFailure err
          Right () -> return ()
  where preludifyModule (Module _ ds) = Module "Prelude" ds
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
  lintCoreInDir "test/desugar"
  describe "substitution" $ do
    describe "in nested bindings" $ do
      it "respects let statements" $ do
        let orig = Let (NonRec (mkVar "a") (Lit $ Integer 1)) (C.Var $ mkVar "b")
        substitute (Internal "a", C.Var $ mkVar "c") orig `shouldBe` orig
      it "respects lambdas" $ do
        let orig = C.Lambda (mkVar "a") (C.Var $ mkVar "b")
        substitute (Internal "a", C.Var $ mkVar "c") orig `shouldBe` orig

    it "substitutes free variables in binders" $ do
      let orig = Let (NonRec (mkVar "a") (C.Var $ mkVar "free")) (C.Var $ mkVar "b")
      let subd = Let (NonRec (mkVar "a") (C.Var $ mkVar "c"))    (C.Var $ mkVar "b")
      substitute (Internal "free", C.Var $ mkVar "c") orig `shouldBe` subd
    it "substitutes free vars in lambda" $ do
      let orig = C.Lambda (mkVar "a") (C.Var $ mkVar "b")
      substitute (Internal "b", C.Var $ mkVar "c") orig `shouldNotBe` orig
    it "substitutes free vars in let" $ do
      let orig = Let (NonRec (mkVar "a") (Lit $ Integer 1)) (C.Var $ mkVar "b")
      substitute (Internal "b", C.Var $ mkVar "c") orig `shouldNotBe` orig
    it "substitutes free vars in case" $ do
      let boundVar   = C.Var $ Id (Internal "bound")   (TVar $ Internal "a") Used
          boundVar2  = C.Var $ Id (Internal "bound2")  (TVar $ Internal "a") Used
          freeVar    = C.Var $ Id (Internal "free")    (TVar $ Internal "a") Used
          changedVar = C.Var $ Id (Internal "changed") (TVar $ Internal "a") Used

      let orig = C.Case boundVar [TrivialAlt $ freeVar :: Alt C.Var]
      let subd = C.Case boundVar [TrivialAlt $ changedVar :: Alt C.Var]
      substitute (Internal "free", changedVar) orig `shouldBe` subd

      let orig = C.Case boundVar [TrivialAlt $ boundVar2, ConAlt (Internal "con") [mkVar "free"] (freeVar)]
      let subd = C.Case boundVar [TrivialAlt $ boundVar2, ConAlt (Internal "con") [mkVar "free"] (freeVar)]
      substitute (Internal "free", changedVar) orig `shouldBe` subd

      let orig = C.Case boundVar [TrivialAlt $ boundVar2, ConAlt (Internal "con") [mkVar "binder"] (freeVar)]
      let subd = C.Case boundVar [TrivialAlt $ boundVar2, ConAlt (Internal "con") [mkVar "binder"] (changedVar)]
      substitute (Internal "free", changedVar) orig `shouldBe` subd


  describe "desugaring" $ do
    it "passes local dicts to constrained methods" localDictsPassedToConstrainedMethods
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
          x = last (moduleDecls typed)
          result = simplifyPatterns x
          expected = [modQ|
            module X
            data L a = C a (L a) | Nil
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
            end
          |]
          Right expected' = bindingGroups expected >>= renameModule
          mappairs = last $ fromBindingGroups . valueDecls $ moduleDecls expected'
      renderIll' (pretty result) `shouldBe` renderIll' (pretty mappairs)

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
          x = last (moduleDecls typed)
          matcher = runFresh $ match [Internal "u"] (declToEqns x)
          result  = matcher (undefined :< Var (Qualified "Prelude" "failedPattern"))
          expected = [modQ|
            module X
              data L a = C a (L a) | Nil
              fn x (u)
                case u of
                  when C a as: 1
                  when Nil: 2
                  when _ : failedPattern
                end
              end
            end
          |]
          Right expected' = bindingGroups expected >>= renameModule
          _ :< Value _ [([_], expectedExpr)] = last $ fromBindingGroups . valueDecls $ moduleDecls expected'
          in renderIll' (pretty result) `shouldBe` renderIll' (pretty expectedExpr)

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
      x = last (moduleDecls typed)
      result = simplifyPatterns x
      expected = [modQ|
        module X
        data L a = C a (L a) | Nil
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
        end
      |]
      Right expected' = bindingGroups expected >>= renameModule
      mappairs = last $ fromBindingGroups . valueDecls $ moduleDecls expected'

  renderIll' (pretty result) `shouldBe` renderIll' (pretty mappairs)

localDictsPassedToConstrainedMethods = do
  let
    mod = [modQ|
      module X
        data Bool = True | False
        trait A a
          test :: a -> Bool
        end

        fn aliased(x)
          test(x)
        end
      end
    |]
    Right (typed, e') = runTC mod
    x = last (moduleDecls typed)
    Module _ [result] = desugarTraits e' (Module "fake" [x])
    expected = [modQ|
      module X
      trait A a
        test :: a -> b
      end
      fn aliased(dict1, x)
        test(dict1)(x)
      end
      end
    |]
    Right expected' = bindingGroups expected >>= renameModule
    expectedDecl = last $ fromBindingGroups . valueDecls $ moduleDecls expected'

  renderIll' (pretty result) `shouldBe` renderIll' (pretty expectedDecl)
