{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances  #-}
module Ill.Syntax.Type
( Type(..)
) where

import Ill.Syntax.Pretty
import Control.Monad.Unify (Unknown)

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
  pretty (TAp f a) = parens $ go f <+> pretty a
    where go (TAp f' a') = go f' <+> pretty a'
          go a'          = pretty a'
  pretty (TConstructor cons) = text cons
  pretty (Arrow from to) = parensIf (complex from) (pretty from) <+> text "->" <+> parensIf (complex to) (pretty to)
  pretty (Trait nm tp) = text nm <+> pretty tp
  pretty (Constraint trts tp) = alternative (map pretty trts) <+> pretty tp
    where alternative = encloseSep empty (empty <+> char '|') (char ',')
  pretty (TUnknown u) = text "unknown" <+> text (show u)

complex :: Type t -> Bool
complex (TConstructor _) = False
complex (Arrow _ _) = True
complex _ = False
