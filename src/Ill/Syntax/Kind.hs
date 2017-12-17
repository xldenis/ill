{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Ill.Syntax.Kind where

import Ill.Prelude

import Control.Monad.Unify (Unknown)
import Ill.Syntax.Pretty
import Data.Data

data Kind
  = Star
  | KFn Kind Kind
  | KUnknown Unknown
  deriving (Show, Eq, Data)

instance Pretty Kind where
  pretty (Star) = text "*"
  pretty (KFn f a) = pretty f <+> text "->" <+> parensIf (complex a) (pretty a)
    where complex (KFn _ _) = True
          complex a         = False
