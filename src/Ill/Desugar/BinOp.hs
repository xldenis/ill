{-# LANGUAGE OverloadedStrings #-}
module Ill.Desugar.BinOp where

import           Ill.Prelude

import           Control.Lens      hiding ((:<))
import           Control.Lens.Plated

import           Ill.Syntax
import           Data.Bifunctor

desugarBinOps :: Module (Qualified Name) TypedAnn -> Module (Qualified Name) TypedAnn
desugarBinOps (Module nm decls) = Module nm (map desugarDecl decls)

desugarDecl :: Decl (Qualified Name) TypedAnn -> Decl (Qualified Name) TypedAnn
desugarDecl (a :< Value n eqns) = a :< (Value n $ transformOn (each . _2) desugarExpr eqns)
desugarDecl (ann :< TraitImpl a b c m) = ann :< (TraitImpl a b c (map desugarDecl m))
desugarDecl a = a

operatorDesugarTable = fmap (bimap f f) $
  [ ("+",  "plus")
  , ("-",  "minus")
  , (">",  "gt")
  , ("<",  "lt")
  , ("==", "eq")
  , ("<=", "leq")
  , (">=", "geq")
  , ("&&", "and")
  , ("||", "or")
  , ("*",  "times")
  ]
  where f = Qualified "Prelude"

desugarExpr (binAnn :< BinOp (v :< Var nm) a b) = case nm `lookup` operatorDesugarTable of
  Just nm' -> binAnn :< Apply (v :< Var nm') [a, b]
  Nothing  -> binAnn :< Apply (v :< Var nm) [a, b]
desugarExpr a = a
