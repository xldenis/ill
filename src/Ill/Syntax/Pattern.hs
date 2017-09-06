{-# LANGUAGE FlexibleInstances, DeriveFunctor #-}
module Ill.Syntax.Pattern where

import Ill.Syntax.Pretty
import Ill.Syntax.Literal

import           Control.Comonad.Cofree

data Pattern a
  = Destructor String [a]
  | Wildcard
  | PVar String
  | PLit Literal
  deriving (Eq, Show, Functor)

type Pat a = Cofree Pattern a

type Patterns a = [Pat a]

instance Pretty (Cofree Pattern a) where
  pretty (_ :< Destructor cons args) = pretty cons <+> hsep (map (\a -> parensIf (complex a) (pretty a)) args)
    where complex (_ :< Destructor _ _) = True
          complex _ = False
  pretty (_ :< Wildcard) = pretty "_"
  pretty (_ :< PVar x) = pretty x

patternNames :: Pat a -> [String]
patternNames (_ :< PVar n) = [n]
patternNames (_ :< Destructor _ pats) = concatMap patternNames pats
patternNames (_ :< Wildcard) = []
