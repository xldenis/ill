module Ill.Desugar.BinOp where

import           Ill.Prelude

import           Control.Lens      hiding ((:<))
import Control.Lens.Plated

import           Ill.Syntax

desugarBinOps :: Module TypedAnn -> Module TypedAnn
desugarBinOps (Module nm decls) = Module nm (map desugarDecl decls)

desugarDecl :: Decl TypedAnn -> Decl TypedAnn
desugarDecl (a :< Value n eqns) = a :< (Value n $ transformOn (each . _2) desugarExpr eqns)
desugarDecl (ann :< TraitImpl a b c m) = ann :< (TraitImpl a b c (map desugarDecl m))
desugarDecl a = a

operatorDesugarTable =
  [ ("+", "plus")
  , ("-", "minus")
  , (">", "gt")
  , ("<", "lt")
  , ("==", "eq")
  , ("<=", "leq")
  , (">=", "geq")
  , ("&&", "and")
  , ("||", "or")
  , ("*", "times")
  , ("*", "times")
  ]

desugarExpr (binAnn :< BinOp (v :< Var nm) a b) = case nm `lookup` operatorDesugarTable of
  Just nm' -> binAnn :< Apply (v :< Var nm') [a, b]
  Nothing  -> binAnn :< Apply (v :< Var nm) [a, b]
desugarExpr a = a
