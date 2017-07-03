module Ill.InferSpec where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Unify

import Test.Hspec
import Text.Megaparsec

import System.Directory
import System.FilePath

import SpecHelper

import Ill.Error
import Ill.Infer
import Ill.Infer.Monad
import Ill.Syntax
import Ill.Parser (illParser)
import Ill.Desugar

spec :: Spec
spec = do
  filesShouldCheck "test/typechecker"
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

runTC :: Check a -> Either MultiError (a, CheckState)
runTC t = runExcept $ runStateT (runCheck t) defaultCheckEnv

filesShouldCheck :: FilePath -> Spec
filesShouldCheck dir = do
  fs <- runIO $ getFilesInDir dir

  describe ("successfully typechecks files in " ++ dir) $ do
    forM_ fs $ \f -> do
      it ((takeFileName f) ++ " typechecks.") $ do
        res <- parseFromFile (illParser <* eof) f
        shouldSucceed res
        let Right (Module _ ds) = res
        case runTC (typeCheck $ bindingGroups ds) of
          Right _ -> return ()
          Left err -> expectationFailure $
            "module should have typechecked but instead returned: " ++ show err
