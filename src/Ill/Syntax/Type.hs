module Ill.Syntax.Type
( Type(..)
) where

  import Ill.Syntax.Pretty

  data Type
    = TVar String
    | Constructor String [Type]
    | Arrow Type Type
    | Trait String Type
    | Constraint [Type] Type
    deriving (Show)

  instance Pretty Type where
    pretty (TVar var) = pretty var
    pretty (Constructor cons args) = text cons <+> (cat $ punctuate comma (map (\a -> parensIf (complex a) $ pretty a) args))
    pretty (Arrow from to) = parensIf (complex from) (pretty from) <+> (text "->") <+> parensIf (complex to) (pretty to)
    pretty (Trait nm tp) = text nm <+> (pretty tp)
    pretty (Constraint trts tp) = (alternative $ map pretty trts) <+> pretty tp
      where alternative = encloseSep empty (empty <+> char '|') (char ',')

  complex :: Type -> Bool
  complex (Constructor _ _) = True
  complex (Arrow _ _) = True
  complex _ = False
