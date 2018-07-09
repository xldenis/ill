{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Thrill.Quote where

import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax

import           Thrill.BindingGroup
import           Thrill.Infer
import           Thrill.Parser
import           Thrill.Parser.Declaration
import           Thrill.Parser.Expression
import           Thrill.Parser.Type
import           Thrill.Parser.Lexer (sc, scn)
import           Thrill.Syntax                 as S

import           Control.Monad.Except       (runExcept)
import           Control.Monad.State        (runStateT)
import           Data.Function
import           Data.Text
import           Text.Megaparsec

import           Data.Data

import           Text.Megaparsec.Pos

unCheck c = runExcept $ runStateT (runCheck c) defaultCheckEnv

deriving instance Data SourceSpan
deriving instance (Data a, Data nm) => Data (S.Module nm a)
deriving instance (Data a, Data b, Data nm) => Data (Declaration nm a b)
deriving instance (Data a, Data b, Data nm) => Data (Expression nm a b)
deriving instance (Data nm, Data a) => Data (S.Pattern nm a)
deriving instance Data Masks

modQ :: QuasiQuoter
modQ = QuasiQuoter
  { quoteExp = \str -> case runParser (scn *> thrillParser) "" (pack str) of
      Left err -> error $ parseErrorPretty err
      Right m  -> liftData m
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }

expr :: QuasiQuoter
expr = QuasiQuoter
  { quoteExp = \str -> case runParser (scn *> nonBodyExpr) "" (pack str) of
      Left err -> error $ parseErrorPretty err
      Right m  -> liftData m
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }

decl :: QuasiQuoter
decl = QuasiQuoter
  { quoteExp = \str -> case runParser (scn *> declaration) "" (pack str) of
      Left err -> error $ parseErrorPretty err
      Right m  -> liftData m
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }

ty :: QuasiQuoter
ty = QuasiQuoter
  { quoteExp = \str -> case runParser (scn *> fullType) "" (pack str) of
      Left err -> error $ parseErrorPretty err
      Right m  -> liftData m
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }
