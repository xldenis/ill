module Ill.Syntax.Literal where
import Ill.Syntax.Pretty

data Literal
  = RawString String
  | EscString String
  | Integer Integer
  | Double Double
  deriving (Eq, Show, Ord)

instance Pretty Literal where
  pretty (RawString x) = squotes (pretty x)
  pretty (EscString x) = dquotes (pretty x)
  pretty (Integer x) = pretty x
  pretty (Double x) = pretty x
