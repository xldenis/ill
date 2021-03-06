{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Thrill.InterpSpec where

import Thrill.Quote
import Text.Megaparsec (runParser)

import Test.Hspec

import SpecHelper

import Thrill.Syntax hiding (Expression(..))
import Thrill.Infer
import Thrill.Infer.Monad as M

import Thrill.Desugar.Cases
import Thrill.Desugar.Trait
import Thrill.Desugar
import Thrill.Syntax.Pretty
import Thrill.BindingGroup
import Thrill.Syntax.Core as Core
import Thrill.Interpret
import Thrill.Renamer

import Control.Monad.State
import Control.Monad.Except (runExcept)
import Control.Monad.IO.Class
import Control.Monad

import Data.Text.Lazy.IO as T
import Data.Maybe
import Data.Bifunctor
import Data.List (find)

spec :: Spec
spec = do
  describe "interprets" $ do

    it "primops" $ do
      let mod = [modQ|
        module Prelude
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
          module Prelude
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
        module Prelude
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

runInterpreter mod =  case bindingGroups mod >>= renameModule >>= typeCheckModule of
  Right (typed, e) -> do
    let desugared = defaultPipeline e typed

    let mod = compileCore desugared
    let boundConstructors = map (fmap consArity) (Core.constructors mod)

    env <- mkEnvForModule boundConstructors (bindings mod)

    eval env (Var $ Id (Qualified (moduleName typed) "main") tNil Used)
  Left err -> error . show $ prettyError err
