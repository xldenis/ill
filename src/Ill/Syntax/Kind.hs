{-# LANGUAGE OverloadedStrings #-}
module Ill.Syntax.Kind where

import Control.Monad.Unify (Unknown)
import Ill.Syntax.Pretty

data Kind
  = Star
  | KFn Kind Kind
  | KUnknown Unknown
  deriving (Show, Eq)

instance Pretty Kind where
  pretty (Star) = text "*"
  pretty (KFn f a) = pretty f <+> text "->" <+> parensIf (complex a) (pretty a)
    where complex (KFn _ _) = True
          complex a         = False
