{-# LANGUAGE OverloadedStrings, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, DeriveDataTypeable #-}
module Ill.Quote where

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import Ill.Syntax as S
import Ill.Parser
import Ill.Desugar
import Ill.Infer

import Control.Monad.State (runStateT)
import Control.Monad.Except (runExcept)
import Text.Megaparsec
import Data.Function
import Data.Text

import Data.Data

unCheck c = runExcept $ runStateT (runCheck c) defaultCheckEnv


-- deriving instance (Data a, Typeable f) => Data (Cofree f a)
-- deriving instance (Data a, Data b) => Data (Declaration a b)
-- deriving instance (Data a) => Data (Expression a)
-- deriving instance Data a => Data (S.Type a)
-- deriving instance Data Pattern
-- deriving instance Data Masks
-- deriving instance Data Literal


-- expr :: QuasiQuoter
-- expr = QuasiQuoter
--   { quoteExp = \str -> case runParser illParser "" (pack str) of
--       Left err -> error $ show err
--       Right m -> liftData m
--   , quotePat = undefined
--   , quoteType = undefined
--   , quoteDec = undefined
--   }

-- expr :: QuasiQuoter
-- expr = QuasiQuoter
--   { quoteExp = \str -> do
--     let e = case runParser illParser "" (pack str) of
--             Left err -> error $ show err
--             Right (S.Module _ ds) -> bindingGroups ds >>= typeCheck & unCheck
--     case e of
--       Left err -> error $ show err
--       Right exp -> return exp
--   , quotePat = undefined
--   , quoteType = undefined
--   , quoteDec = undefined
--   }