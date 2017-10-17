{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Ill.InterpSpec where

import Ill.Quote
import Text.Megaparsec (runParser)

import Test.Hspec

import SpecHelper

import Ill.Syntax
import Ill.Infer
import Ill.Infer.Monad as M

import Ill.Desugar.Cases
import Ill.Desugar.Trait
import Ill.Desugar
import Ill.Syntax.Pretty
import Ill.BindingGroup
import Ill.Syntax.Core
import Ill.Interpret

import Control.Monad.State
import Control.Monad.Except (runExcept)
import Control.Monad.IO.Class
import Control.Monad

import Data.Text.Lazy.IO as T
import Data.Maybe
import Data.Bifunctor
import Data.List (find)

runTC (Module _ ds) = unCheck (bindingGroups ds >>= typeCheck) >>= pure . bimap fromBindingGroups env

-- unCheck c = runExcept $ runStateT (runCheck c) defaultCheckEnv

spec :: Spec
spec = do
  describe "interprets" $ do

    it "primops" $ do
      let mod = [modQ|
        module X
          trait Semigroup a
            plus :: a -> a -> a
          end

          impl (Semigroup Int)
            fn plus ()
              plusInt
            end
          end

          fn main ()
            1+1
          end
        end
      |]

      case runInterpreter mod of
        Right res -> res `shouldBe` (Lit $ Integer 2)
        Left err -> expectationFailure $ err
    describe "adt" $ do
      it "matches" $ do
        let mod = [modQ|
          module X
            data M a = M a

            fn main ()
              x = M(20)
              case x of
                when M y: y
              end
            end
          end
        |]
        case runInterpreter mod of
          Right res -> res `shouldBe` (Lit $ Integer 20)
          Left err -> expectationFailure $ err
      it "matches complex patterns" $ do
        let mod = [modQ|
          module X
            trait Semigroup a
              plus :: a -> a -> a
            end

            impl (Semigroup Int)
              fn plus ()
                plusInt
              end
            end

            data L a = C a (L a) | Nil

            fn sum (C a as)
              a + sum(as)
            or sum (Nil)
              0
            end

            fn main ()
              list = C(5, C(2, C(15, Nil)))

              sum(list)
            end
          end
        |]

        case runInterpreter mod of
          Right res -> res `shouldBe` (Lit $ Integer 22)
          Left err -> expectationFailure $ err

    it "basic command works" $ do
      let mod = [modQ|
        module X
          data L a = C a (L a) | Nil
          fn a (C a as)
            1
          or a (Nil)
            2
          end

          fn main ()
            list = Nil
            a(list)
          end
        end
      |]

      case runInterpreter mod of
          Right res -> res `shouldBe` (Lit $ Integer 2)
          Left err -> expectationFailure $ err

getConstructorArities (_ :< Data nm _ conses) = map (\cons ->
  case unwrapProduct cons of
    (TConstructor consNm : args) -> (consNm, length args)
  ) conses
getConstructorArities _ = []

runInterpreter mod =  case runTC mod of
  Right (typed, e) -> do
    let desugaringPipe = (desugarTraits e . desugarBinOps) >=> pure . simplifyPatterns
        desugared = desugaringPipe typed
    let core = declToCore desugared
        boundConstructors = desugared >>= getConstructorArities
        context = Context (map (\(NonRec n e) -> (name n, e)) core) boundConstructors []
        NonRec _ mainExpr = fromJust $ find (\(NonRec n _) -> (name n) == "main") core
    runExcept . (flip evalStateT context) $ interpret mainExpr
