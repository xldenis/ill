{-# LANGUAGE DeriveFunctor #-}
module Ill.Syntax.Expression where
  import Ill.Syntax.Pretty

  import Ill.Syntax.Pattern
  import Ill.Syntax.Literal

  data Expression a
    = Apply a [a]
    | Assign [String] [a]
    | Case a [(Pattern, a)]
    | If a a a
    | Lambda [Pattern] a
    | Var String
    | Literal Literal
    | Body [a]
    | Hash [(a, a)]
    | Array [a]
    deriving (Functor, Show)


  instance Pretty a => Pretty (Expression a) where
    --pretty (Apply x1 x2) = _
    pretty (Assign idents exprs) = (cat $ punctuate comma (map text idents)) <+> (char '=') <+> (cat $ punctuate comma (map pretty exprs))
    --pretty (Case x1 x2) = _
    --pretty (If x1 x2 x3) = _
    --pretty (Lambda x1 x2) = _
    pretty (Var v) = text v
    pretty (Literal l) = pretty l
    pretty (Body body) = vsep (map pretty body)
    --pretty (Hash x) = _
    --pretty (Array x) = _
