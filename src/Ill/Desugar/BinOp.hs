module Ill.Desugar.BinOp where

import           Control.Lens      hiding ((:<))
import Control.Lens.Plated

import           Ill.Syntax

desugarBinOps :: [Decl TypedAnn] -> [Decl TypedAnn]
desugarBinOps = map desugarDecl

desugarDecl :: Decl TypedAnn -> Decl TypedAnn
desugarDecl (a :< Value n eqns) = a :< (Value n $ transformOn (each . _2) desugarExpr eqns)
desugarDecl (ann :< TraitImpl a b c m) = ann :< (TraitImpl a b c (map desugarDecl m))
desugarDecl a = a


operatorDesugarTable =
  [ ("+", "plus")
  , ("-", "minus")
  , (">", "gt")
  , ("<", "lt")
  , ("&&", "and")
  , ("||", "or")
  , ("*", "times")
  , ("*", "times")
  ]

desugarExpr (binAnn :< BinOp (v :< Var nm) a b) = case nm `lookup` operatorDesugarTable of
  Just nm' -> binAnn :< Apply (v :< Var nm') [a, b]
  Nothing  -> binAnn :< Apply (v :< Var nm) [a, b]
desugarExpr a = a
