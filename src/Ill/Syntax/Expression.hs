{-# LANGUAGE DeriveFunctor, FlexibleInstances #-}
module Ill.Syntax.Expression where
  import Control.Comonad.Cofree
  import Control.Comonad

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

  type Expr a = Cofree Expression a

  instance Pretty (Cofree Expression a) where
    pretty (_ :< f) = pretty' f where
      pretty' (Apply func args) = pretty func <> tupled (map pretty args)
      pretty' (Assign idents exprs) = cat (punctuate comma (map text idents)) <+> char '=' <+> cat (punctuate comma (map pretty exprs))
      --pretty' (Case x1 x2) = _
      pretty' (If cond left right) =
        text "if" <+> pretty cond <+> text "then"
        </> indent 2 (pretty left)
        </> text "else"
        </> indent 2 (pretty right)
        </> text "end"
      pretty' (Lambda args body) = text "fn" <+> tupled (map pretty args) `above` pretty body `above` text "end"
      pretty' (Var v) = text v
      pretty' (Literal l) = pretty l
      pretty' (Body body) = nest 2 $ vsep (map pretty body)
      --pretty' (Hash x) = _
      pretty' (Array ar) = list (map pretty ar)

  newtype Mu f = Mu (f (Mu f))
