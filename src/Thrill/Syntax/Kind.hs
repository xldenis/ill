{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Thrill.Syntax.Kind where

import Thrill.Prelude

import Control.Monad.Unify (Unknown)
import Thrill.Syntax.Pretty
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
