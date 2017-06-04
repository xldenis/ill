module Ill.Desugar where

data Core
  = Let
  | Lam
  | Ap
  | Var
  | Case
  deriving (Show, Eq)
