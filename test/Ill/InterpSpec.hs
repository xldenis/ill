{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Ill.InterpSpec where

import Ill.Quote
import Text.Megaparsec (runParser)

import Test.Hspec

import SpecHelper

import Ill.Syntax hiding (Expression(..))
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

      result <- runInterpreter mod
      case result of
        VLit l -> l `shouldBe` (Integer 2)
        _ -> expectationFailure "a literal value should have been produced"

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
        result <- runInterpreter mod
        case result of
          VLit l -> l `shouldBe` (Integer 20)
          _ -> expectationFailure "a literal value should have been produced"

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

        result <- runInterpreter mod
        case result of
          VLit l -> l `shouldBe` (Integer 22)
          _ -> expectationFailure "a literal value should have been produced"

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

      result <- runInterpreter mod
      case result of
        VLit l -> l `shouldBe` (Integer 2)
        _ -> expectationFailure "a literal value should have been produced"

getConstructorArities (_ :< Data nm _ conses) = map (\cons ->
  case unwrapProduct cons of
    (TConstructor consNm : args) -> (consNm, length args)
  ) conses
getConstructorArities _ = []

runInterpreter mod =  case runTC mod of
  Right (typed, e) -> do
    let desugaringPipe = (desugarTraits e . desugarBinOps) >=> pure . simplifyPatterns
        desugared = desugaringPipe typed

    let (Mod core coreConstructors _) = declsToCore desugared
    let boundConstructors = map (fmap consArity) $ coreConstructors

    env <- mkEnvForModule boundConstructors core

    eval env (Var $ Id "main" tNil Used)
