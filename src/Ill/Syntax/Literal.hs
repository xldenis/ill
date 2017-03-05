module Ill.Syntax.Literal where
import Ill.Syntax.Pretty

data Literal
  = RawString String
  | EscString String
  | Integer Integer
  | Double Double
  deriving (Eq, Show)

instance Pretty Literal where
  pretty (RawString x) = squotes (text x)
  pretty (EscString x) = dquotes (text x)
  pretty (Integer x) = pretty x
  pretty (Double x) = pretty x
