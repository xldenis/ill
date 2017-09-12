{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Ill.Quote where

import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax

import           Ill.Desugar
import           Ill.Infer
import           Ill.Parser
import           Ill.Parser.Declaration
import           Ill.Parser.Expression
import           Ill.Parser.Type
import           Ill.Parser.Lexer (sc, scn)
import           Ill.Syntax                 as S

import           Control.Monad.Except       (runExcept)
import           Control.Monad.State        (runStateT)
import           Data.Function
import           Data.Text
import           Text.Megaparsec

import           Data.Data

import           Text.Megaparsec.Pos

unCheck c = runExcept $ runStateT (runCheck c) defaultCheckEnv

deriving instance Data SourceSpan
deriving instance Data a => Data (S.Module a)
deriving instance (Data a) => Data (Decl a)
deriving instance (Data a, Data b) => Data (Declaration a b)
deriving instance (Data a, Data b) => Data (Expression a b)
deriving instance (Data a) => Data (Expr a)
deriving instance Data a => Data (S.Type a)
deriving instance Data a => Data (S.Pat a)
deriving instance Data a => Data (S.Pattern a)
deriving instance Data Masks
deriving instance Data Literal


modQ :: QuasiQuoter
modQ = QuasiQuoter
  { quoteExp = \str -> case runParser (scn *> illParser) "" (pack str) of
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
