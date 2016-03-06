{-# LANGUAGE DeriveAnyClass, TemplateHaskell, DeriveFunctor, DeriveTraversable #-}
module Simple where

import Prelude.Extras (Read1, Show1)


import Data.Bitraversable
import Data.Bifunctor
import Data.Bifoldable

import Bound
import Bound.Name (Name)
import Bound.Scope    (bitraverseScope)

type = Name String Int
data Exp ann a
  = V a
  | App (Exp ann a) (Exp ann a)
  | Lam (Scope N (Exp ann) a)
  | I Int
  deriving (Functor, Read, Show, Traversable, Read1, Show1, Foldable)

instance Monad (Exp t) where
  return  = V
  a >>= b = bindTerm id b a

instance Applicative (Exp t) where
  pure = V
  (<*>) = ap

instance Bitraversable Exp where
  bitraverse f g = t where
    t (V a)     = V <$> g a
    t (App l r) = App <$> t l <*> t r
    t (Lam s)   = Lam <$> bitraverseScope f g s
    t (I i)     = pure (I i)

instance Bifoldable Exp where
  bifoldMap = bifoldMapDefault

instance Bifunctor Exp where
  bimap = bimapDefault

bindTerm :: (ann -> ann') -> (a -> Exp ann' b) -> Exp ann a -> Exp ann' b
bindTerm _ g (V a) = g a
bindTerm f g (App l r) = App (bindTerm f g l) (bindTerm f g r)
bindTerm f g (Lam (Scope b)) = Lam (Scope (bimap f (fmap (bindTerm f g)) b))
