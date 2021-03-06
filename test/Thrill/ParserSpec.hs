module Thrill.ParserSpec where

import Control.Applicative ((<*))
import Text.Megaparsec (many)

import Thrill.Parser.Lexer (scn)
import Thrill.Parser
import Thrill.Parser.Declaration

import Test.Hspec

import SpecHelper

import Control.Monad (forM_)

import Text.Megaparsec (runParser)
import Data.Text.Lazy (pack, unpack)
import Data.Text.Lazy (toStrict)

import Thrill.Syntax.Pretty (renderThrill, defaultRenderArgs, pretty)
import Thrill.Syntax (Module, Module'(..), dropAnn)

import Control.Comonad (extend)

spec :: Spec
spec = parallel $ do
  filesShouldParse "test/parser/success" thrillParser
  filesShouldParse "test/parser/success/declaration" (many $ declaration <* scn)
  filesShouldFail  "test/parser/failure/declaration" (declaration)
  describe "pretty printer output parses" $ do
    fs <- runIO $ getFilesInDir "test/parser/success"
    forM_ fs $ \file -> do
      it (file ++ " pretty prints correctly") $ do
        p <- parseFromFile thrillParser file
        shouldSucceed p
        let Right ast = p

        propPrettyParse file ast

propPrettyParse f ast = do
  let result = (parse . toStrict $ prettyText ast)
  case result of
    Right ast' | (noPos ast') == (noPos ast) -> return ()
    Right ast' -> do
      expectationFailure "pretty printed output parsed but was not equivalent"
    Left  err -> do
      expectationFailure $ (unpack $ prettyText ast) ++ "\n\n" ++ (showParseError err)
  where parse = runParser thrillParser f
        prettyText a = renderThrill defaultRenderArgs (pretty a)
        noPos :: Module nm a -> Module nm ()
        noPos (Module n ds) = Module n $ map (dropAnn) ds
