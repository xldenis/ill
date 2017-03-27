module Ill.Syntax.Literal where
import Ill.Syntax.Pretty

import Ill.Inference.Type as T

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

tInteger = TCon (Tycon "Integer" Star)
tDouble  = TCon (Tycon "Double" Star)
tChar    = TCon (Tycon "Char" Star)
tBool    = TCon (Tycon "Bool" Star)

tString    :: Type
tString     = T.list tChar

tiLit :: Literal -> TI ([Pred], Type)
tiLit (RawString _) = return ([], tString)
tiLit (EscString _) = return ([], tString)
tiLit (Integer   _) = return ([], tInteger)
tiLit (Double    _) = return ([], tDouble)
