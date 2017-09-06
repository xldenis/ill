{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances, DeriveAnyClass #-}
module Ill.Syntax.Expression where
import           Control.Comonad.Cofree
import           Ill.Syntax.Pretty

import           Ill.Syntax.Literal
import           Ill.Syntax.Pattern

import Data.Bifunctor

data Expression p a
  = Apply a [a]
  | BinOp a a a
  | Assign [String] [a]
  | Case a [(Pat p, a)]
  | If a a a
  | Lambda [Pat p] a
  | Var String
  | Constructor String
  | Literal Literal
  | Body [a]
  -- | Hash [(a, a)]
  | Array [a]
  deriving (Eq, Functor, Show, Bifunctor)

type Expr a = Cofree (Expression a) a

instance Pretty (Expr a) where
  prettyList es = vsep $ (map pretty es)
  pretty (_ :< f) = pretty' f where
    pretty' (Apply func args) = pretty func <> tupled (map pretty args)
    pretty' (BinOp op l r) = pretty l <+> pretty op <+> pretty r
    pretty' (Assign idents exprs) = cat (punctuate comma (map pretty idents)) <+> pretty '=' <+> cat (punctuate comma (map pretty exprs))
    pretty' (Case cond branches) = pretty "case" <+> pretty cond <+> pretty "of" `above` indent 2 (vsep (map prettyBranch branches)) `above` pretty "end"
      where prettyBranch (pat, branch) = pretty "when" <+> pretty pat <+> pretty ":" <+> pretty branch
    pretty' (If cond left right) = vsep
      [ pretty "if" <+> pretty cond <+> pretty "then"
      , indent 2 (pretty left)
      , pretty "else"
      , indent 2 (pretty right)
      , pretty "end"
      ]
    pretty' (Lambda args body) = pretty "fn" <+> tupled (map pretty args) `above` pretty body `above` pretty "end"
    pretty' (Var v) = pretty v
    pretty' (Constructor c) = pretty c
    pretty' (Literal l) = pretty l
    pretty' (Body body) = prettyList body
    --pretty' (Hash x) = _
    pretty' (Array ar) = list (map pretty ar)

