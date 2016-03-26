module Ill.Syntax.Pattern where
  import Ill.Syntax.Pretty

  data Pattern
    = Destructor String [Pattern]
    | Wildcard
    | PVar String
    | Nil
    deriving (Show)

  instance Pretty Pattern where
    pretty (Destructor cons args) = text cons <+> hsep (map (\a -> parensIf (complex a) (pretty a)) args)
      where complex (Destructor _ _) = True
            complex _ = False
    pretty Wildcard = text "_"
    pretty (PVar x) = text x
    pretty Nil = empty

