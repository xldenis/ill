module Ill.InferSpec where
import SpecHelper

import Test.Hspec
import Text.Megaparsec

import Ill.Infer
import Control.Monad.Unify

import Ill.Syntax

import Control.Monad.State
import Control.Monad.Except
import Ill.Infer.Monad

spec :: Spec
spec = do
  describe "unifyTypes" $ do
    it "" $ do
      let tc = runTC $ do
                u <- liftUnify $ do
                  tvar1 <- fresh

                  let t1 = tvar1 `tFn` tvar1
                      t2 = tInteger `tFn` tInteger

                  t1 =?= t2
                return $ fst u
      (fst <$> tc) `shouldBe` Right ()


runTC :: Check a -> Either String (a, CheckState)
runTC t = runExcept $ runStateT (runCheck t) defaultCheckEnv
