module SpecHelper where

import Control.Monad (filterM)
import Data.Text (Text)
import System.Directory
import System.FilePath

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

filesShouldParse :: Show b => FilePath -> Parsec Text b -> Spec
filesShouldParse dir p = do
  fs <- runIO $ getDirectoryPaths dir >>= filterM (doesFileExist)

  describe ("parser successfully parses files in " ++ dir) $ do
    mapM_ (\f -> do
      it ((takeFileName f) ++ " parses correctly.") $ do
        (parseFromFile (p <* eof) f) >>= shouldSucceed) fs

filesShouldFail :: Show b => FilePath -> Parsec Text b -> Spec
filesShouldFail dir p = do
  fs <- runIO $ getDirectoryPaths dir >>= filterM (doesFileExist)

  describe ("parser fails to parse files in " ++ dir) $ do
    mapM_ (\f -> do
      it ((takeFileName f) ++ " fails.") $ do
        (parseFromFile (p <* eof) f) >>= shouldFail) fs

type ParserExpectation a = Either ParseError a -> Expectation

-- | Expectation that argument is result of a failed parser.

shouldFail :: Show a => ParserExpectation a
shouldFail r = case r of
  Left _ -> return ()
  Right v -> expectationFailure $
    "the parser is expected to fail, but it parsed: " ++ show v

-- | Expectation that argument is result of a succeeded parser.

shouldSucceed :: Show a => ParserExpectation a
shouldSucceed r = case r of
  Left e -> expectationFailure $
    "the parser is expected to succeed, but it failed with:\n" ++
    showParseError e
  Right _ -> return ()

showParseError :: ParseError -> String
showParseError = unlines . fmap ("  " ++) . lines . show

getDirectoryPaths :: String -> IO [FilePath]
getDirectoryPaths dir =  map (\f -> replaceDirectory f dir) <$> getDirectoryContents dir
