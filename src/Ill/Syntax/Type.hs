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
  | Trait t (Type t)
  | Constraint [Type t] (Type t)
  -- | Constraint [Constraint t]
  | TUnknown Unknown
  deriving (Eq, Show)

-- type Constraint t = (t, [Type])

instance Pretty (Type String) where
  pretty (TVar var) = pretty var
  pretty (TAp (TConstructor "->") a) = pretty a <+> (text "->")
  pretty (TAp f a) = pretty f <+> parensIf (complex a) (pretty a)
    where go (TAp f' a') = go f' <+> parensIf (complex a') (pretty a')
          go a'          = pretty a'
  pretty (TConstructor cons) = text cons
  pretty (Arrow from to) = parensIf (complex from) (pretty from) <+> text "->" <+> parensIf (complex to) (pretty to)
  pretty (Trait nm tp) = text nm <+> pretty tp
  pretty (Constraint trts tp) = alternative (map pretty trts) <+> pretty tp
    where alternative = encloseSep empty (empty <+> char '|') (char ',')
  pretty (TUnknown u) = text "unknown" <+> text (show u)

tArrow :: Type String
tArrow = TConstructor "->"

tFn :: Type String -> Type String -> Type String
tFn a b = tArrow `TAp` a `TAp` b

tString :: Type String
tString = TConstructor "String"

tBool = TConstructor "Bool"
tInteger = TConstructor "Int"
tDouble = TConstructor "Double"

complex :: Type t -> Bool
complex (TConstructor _) = False
complex (Arrow _ _) = True
complex (TAp _ _) = True
complex _ = False

varIfUnknown :: Type String -> Type String
varIfUnknown (TAp l r) = TAp (varIfUnknown l) (varIfUnknown r)
varIfUnknown (Arrow l r) = Arrow (varIfUnknown l) (varIfUnknown r)
varIfUnknown (Trait n t) = Trait n (varIfUnknown t)
varIfUnknown (Constraint ts t') = Constraint (map varIfUnknown ts) (varIfUnknown t')
varIfUnknown (TUnknown u) = TVar toName
  where toName = "a" ++ show u
varIfUnknown a = a

replaceVar :: String -> Type String -> Type String -> Type String
replaceVar v t (TAp l r) = TAp (replaceVar v t l) (replaceVar v t r)
replaceVar v t (Arrow l r) = Arrow (replaceVar v t l) (replaceVar v t r)
replaceVar v t (Trait n t') = Trait n (replaceVar v t t')
replaceVar v t (Constraint ts t') = Constraint (map (replaceVar v t) ts) (replaceVar v t t')
replaceVar v t (TVar n) | n == v = t
replaceVar v t a = a

varsInType :: Type String -> [String]
varsInType (TAp l r) = varsInType l ++ varsInType r
varsInType (Arrow l r) = varsInType l ++ varsInType r
varsInType (Trait n t) = varsInType t
varsInType (Constraint ts t') = concatMap varsInType ts ++ varsInType t'
varsInType (TVar t) = [t]
varsInType a = []
