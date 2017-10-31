{-# LANGUAGE FlexibleInstances, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
module Ill.Syntax.Pattern where

import Ill.Syntax.Pretty
import Ill.Syntax.Literal

import           Control.Comonad.Cofree

data Pattern a
  = Destructor String [a]
  | Wildcard
  | PVar String
  | PLit Literal
  deriving (Eq, Show, Functor, Foldable, Traversable)

type Pat a = Cofree Pattern a

type Patterns a = [Pat a]

instance Pretty (Cofree Pattern a) where
  pretty (_ :< Destructor cons args) = pretty cons <-> hsep (map (\a -> parensIf (complex a) (pretty a)) args)
    where complex (_ :< Destructor _ []) = False
          complex (_ :< Destructor _ _)  = True
          complex _ = False
  pretty (_ :< Wildcard) = pretty "_"
  pretty (_ :< PVar x) = pretty x
  pretty (_ :< PLit l) = pretty l

patternNames :: Pat a -> [String]
patternNames (_ :< PVar n) = [n]
patternNames (_ :< Destructor _ pats) = concatMap patternNames pats
patternNames (_ :< Wildcard) = []
