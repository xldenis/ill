{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, PatternSynonyms  #-}
module Ill.Syntax.Type where

import Ill.Syntax.Pretty
import Control.Monad.Unify (Unknown)

-- pattern Arrow a b = (TAp (TAp (TConstructor "->") a) b)

data Type t
  = TVar t
  | TAp (Type t) (Type t)
  | TConstructor t
  | Arrow (Type t) (Type t)
  | Constrained [Constraint t] (Type t)
  | TUnknown Unknown
  deriving (Eq, Show)

type Constraint t = (t, [Type t])

instance Pretty (Type String) where
  pretty (TVar var) = pretty var
  pretty (TAp (TAp (TConstructor "->") a) b) = parensIf (complex a) (pretty a) <+> (pretty "->") <+> (pretty b)
  pretty (TAp f a) = pretty f <+> parensIf (complex a) (pretty a)
  pretty (TConstructor cons) = pretty cons
  pretty (Arrow from to) = parensIf (complex from) (pretty from) <+> pretty "->" <+> parensIf (complex to) (pretty to)
  pretty (Constrained trts tp) = alternative (map prettyCons trts) <+> pretty tp
    where alternative = encloseSep mempty (mempty <+> pretty '|') (pretty ',')
          prettyCons (nm, ts) = pretty nm <+> hsep (map pretty ts)
  pretty (TUnknown u) = pretty "unknown" <+> pretty (show u)

tArrow :: Type String
tArrow = TConstructor "->"

tFn :: Type String -> Type String -> Type String
tFn = Arrow
-- tFn a b = tArrow `TAp` a `TAp` b

tString :: Type String
tString = TConstructor "String"

tBool = TConstructor "Bool"
tInteger = TConstructor "Int"
tDouble = TConstructor "Double"

complex :: Type t -> Bool
complex (Arrow _ _) = False
complex (TAp _ _) = True
complex _ = False

varIfUnknown :: Type String -> Type String
varIfUnknown (TAp l r) = TAp (varIfUnknown l) (varIfUnknown r)
varIfUnknown (Arrow l r) = Arrow (varIfUnknown l) (varIfUnknown r)
varIfUnknown (Constrained ts t') = Constrained (map varIfUnknown' ts) (varIfUnknown t')
  where varIfUnknown' (a, ts) = (a, map varIfUnknown ts)
varIfUnknown (TUnknown u) = TVar toName
  where toName = "a" ++ show u
varIfUnknown a = a

replaceVar :: String -> Type String -> Type String -> Type String
replaceVar v t (TAp l r) = TAp (replaceVar v t l) (replaceVar v t r)
replaceVar v t (Arrow l r) = Arrow (replaceVar v t l) (replaceVar v t r)
replaceVar v t (Constrained ts t') = Constrained (map replaceVar' ts) (replaceVar v t t')
  where replaceVar' (n, ts) = (n, map (replaceVar v t) ts)
replaceVar v t (TVar n) | n == v = t
replaceVar v t a = a

varsInType :: Type String -> [String]
varsInType (TAp l r) = varsInType l ++ varsInType r
varsInType (Arrow l r) = varsInType l ++ varsInType r
varsInType (Constrained ts t') = concatMap varsInType' ts ++ varsInType t'
  where varsInType' (n, ts') = concatMap varsInType ts'
varsInType (TVar t) = [t]
varsInType a = []

unconstrained :: Type String -> ([Constraint String], Type String)
unconstrained (Constrained cons t) = (cons, t)
unconstrained t@(TAp l r) = let 
  (cons1, l') = unconstrained l 
  (cons2, r') = unconstrained r
  in (cons1 ++ cons2, TAp l' r')
unconstrained t@(Arrow l r) = let 
  (cons1, l') = unconstrained l 
  (cons2, r') = unconstrained r
  in (cons1 ++ cons2, Arrow l' r')
unconstrained t = ([], t)

flattenConstraints :: Type String -> Type String
flattenConstraints t = case unconstrained t of
  ([], t) -> t
  (ts, t) -> Constrained ts t

