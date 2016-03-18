module Ill.Syntax.Type
( Type(..)
) where

  import Ill.Syntax.Pretty

  data Type
    = TVar String
    | Constructor String [Type]
    | Arrow Type Type
    deriving (Show)

  instance Pretty Type where
    pretty (TVar var) = pretty var
    pretty (Constructor cons args) = text cons <+> (cat $ punctuate comma (map (\a -> parensIf (complex a) $ pretty a) args))
    pretty (Arrow from to) = parensIf (complex from) (pretty from) <+> (text "->") <+> parensIf (complex to) (pretty to)

  complex :: Type -> Bool
  complex (Constructor _ _) = True
  complex (Arrow _ _) = True
  complex _ = False
