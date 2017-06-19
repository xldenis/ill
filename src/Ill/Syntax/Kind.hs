module Ill.Syntax.Kind where

import Control.Monad.Unify (Unknown)

data Kind
  = Star
  | KFn Kind Kind
  | KUnknown Unknown
  deriving (Show, Eq)
