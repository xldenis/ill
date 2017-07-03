module Ill.Syntax.Pattern where

import Ill.Syntax.Pretty
import Ill.Syntax.Literal

data Pattern
  = Destructor String [Pattern]
  | Wildcard
  | PVar String
  | PLit Literal
  deriving (Eq, Show)

instance Pretty Pattern where
  pretty (Destructor cons args) = pretty cons <+> hsep (map (\a -> parensIf (complex a) (pretty a)) args)
    where complex (Destructor _ _) = True
          complex _ = False
  pretty Wildcard = pretty "_"
  pretty (PVar x) = pretty x

patternNames :: Pattern -> [String]
patternNames (PVar n) = [n]
patternNames (Destructor _ pats) = concatMap patternNames pats
patternNames Wildcard = []
