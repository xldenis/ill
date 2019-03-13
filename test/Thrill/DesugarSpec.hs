{-# LANGUAGE QuasiQuotes #-}
module Thrill.DesugarSpec where

import Thrill.Quote
import Text.Megaparsec (runParser)

import Test.Hspec

import SpecHelper

import Control.Monad.IO.Class

import Thrill.Parser
import Thrill.Syntax
import Thrill.Syntax.Core as C hiding (Var)
import qualified Thrill.Syntax.Core as C
import Thrill.Infer
import Thrill.Infer.Monad (execCheck)
import Thrill.Desugar
import Thrill.Renamer
import Thrill.Syntax.Pretty
import Thrill.BindingGroup
import Thrill.CoreLint
import Thrill.Error (prettyError)

import Data.Text.Lazy.IO as T
import Data.Maybe
import Data.Bifunctor

import Control.Arrow
import Control.Monad

import System.Directory
import System.FilePath

spec :: Spec
spec = do
  describe "desugar cases" $ do
    it "should group literals in same case expression" $ do
      pending
