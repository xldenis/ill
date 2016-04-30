module Ill.Inference.Kind where

  data Kind
    = Star
    | KFun Kind Kind

  data Type = TVar Tyvar | TAp Type Type | TCon Tycon | TGen Int deriving (Show, Eq)

  data Tyvar = Tyvar Id Kind deriving (Show, Eq)

  data Tycon = Tycon Id Kind deriving (Show, Eq)

  typeFromSyntax :: Syntax.Type -> Type


  fn :: Type -> Type -> Type
  fn = TAp . TAp (TCon $ Tycon "->" (KFn Star (KFn Star Star)))

  class HasKind k where
    kind :: k -> Kind

  instance HasKind Tyvar where
    kind (Tyvar _ k) = k

  instance HasKind Tycon where
    kind (Tycon _ k) = k

  instance HasKind Type where
    kind (TVar t) = kind t
    kind (TAp l r) = case kind l of -- l :: k -> k', r :: k => l (r) :: k'
      (KFn k k') -> k'
    kind (TCon c) = kind c
