{-# LANGUAGE FlexibleInstances, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
module Ill.Syntax.Pattern where

import Ill.Prelude

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

instance Pretty1 f => Pretty (Cofree f a) where
  pretty (a :< el) = liftPretty pretty el

instance Pretty1 Pattern where
  liftPretty pretty' (Destructor cons args) = pretty cons <-> hsep (map
    (\a -> let
      doc = pretty' a
    in parensIf (complexDoc doc) doc) args)
  liftPretty pretty' (Wildcard) = pretty "_"
  liftPretty pretty' (PVar x) = pretty x
  liftPretty pretty' (PLit l) = pretty l

patternNames :: Pat a -> [String]
patternNames (_ :< PVar n) = [n]
patternNames (_ :< Destructor _ pats) = concatMap patternNames pats
patternNames (_ :< Wildcard) = []
