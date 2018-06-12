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
import Ill.BindingGroup
import Ill.Renamer

spec :: Spec
spec = do
  filesShouldCheck "test/typechecker"
  filesShouldNotCheck "test/typechecker/failure"
  describe "unifyTypes" $ do
    it "" $ do
      let tc = execCheck $ do
                u <- liftUnify $ do
                  tvar1 <- fresh

                  let t1 = tvar1 `tFn` tvar1
                      t2 = tInteger `tFn` tInteger

                  t1 =?= t2
                return $ fst u

      case fst <$> tc of
        Right () -> return ()
        Left err -> expectationFailure . show $ prettyError err

filesShouldNotCheck :: FilePath -> Spec
filesShouldNotCheck dir = do
  fs <- runIO $ getFilesInDir dir

  describe ("fails to typecheck files in " ++ dir) $ do
    forM_ fs $ \f -> do
      it ((takeFileName f) ++ " errors.") $ do
        res <- parseFromFile (illParser <* eof) f
        shouldSucceed res
        let Right (Module _ ds) = res
            m = Module "Prelude" ds
        case bindingGroups m >>= (renameModule >=> typeCheckModule) of
          Left _ -> return ()
          Right _ -> expectationFailure $
            "module should have errored but instead typechecked."

filesShouldCheck :: FilePath -> Spec
filesShouldCheck dir = do
  fs <- runIO $ getFilesInDir dir

  describe ("successfully typechecks files in " ++ dir) $ do
    forM_ fs $ \f -> do
      it ((takeFileName f) ++ " typechecks.") $ do
        res <- parseFromFile (illParser <* eof) f
        shouldSucceed res
        let Right (Module _ ds) = res
            m = Module "Prelude" ds
        case bindingGroups m >>= (renameModule >=> typeCheckModule) of
          Right _ -> return ()
          Left err -> expectationFailure $
            "module should have typechecked but instead returned: " ++ (show $ prettyError err)
