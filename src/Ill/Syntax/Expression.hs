{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}

module Ill.Syntax.Expression where
import           Control.Lens.TH

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
  deriving (Eq, Functor, Show, Traversable, Foldable)

makePrisms ''Expression

type Expr a = Cofree (Expression a) a

instance Bifunctor Expression where
  bimap l r (Apply a as) = Apply (r a) (map r as)
  bimap l r (BinOp o a b) = BinOp (r o) (r a) (r b)
  bimap l r (Assign s as) = Assign s (map r as)
  bimap l r (Case a brs) = Case (r a) $ map (bimap (fmap l) r) brs
  bimap l r (If c a b) = If (r c) (r a) (r b)
  bimap l r (Lambda ps b) = Lambda (map (fmap l) ps) (r b)
  bimap l r (Var s) = Var s
  bimap l r (Constructor s) = Constructor s
  bimap l r (Literal a) = Literal a
  bimap l r (Body bs) = Body (map r bs)
  bimap l r (Array as) = Array (map r as)

instance Pretty (Expr a) where
  prettyList es = vsep $ (map pretty es)
  pretty (_ :< f) = pretty' f where
    -- Remove usage of tupled which clattens long cases to one line :(
    --
    pretty' (Apply func args) = pretty func <> tupled (map pretty args)
    pretty' (BinOp op l r) = pretty l <+> pretty op <+> pretty r
    pretty' (Assign idents exprs) = cat (punctuate comma (map pretty idents)) <+> pretty '=' <+> cat (punctuate comma (map pretty exprs))
    pretty' (Case cond branches) = align $ vsep'
      [ pretty "case" <+> pretty cond <+> pretty "of"
      , indent 2 (vsep (map prettyBranch branches))
      , pretty "end"
      ]
      where prettyBranch (pat, branch) = pretty "when" <+> pretty pat <+> pretty ":" <+> pretty branch
    pretty' (If cond left right) = vsep
      [ pretty "if" <+> pretty cond <+> pretty "then"
      , indent 2 (pretty left)
      , pretty "else"
      , indent 2 (pretty right)
      , pretty "end"
      ]
    pretty' (Lambda args body) = pretty "fn" <+> tupled (map pretty args) <+> pretty "=" `above` indent 2 (pretty body) `above` pretty "end"
    pretty' (Var v) = pretty v
    pretty' (Constructor c) = pretty c
    pretty' (Literal l) = pretty l
    pretty' (Body body) = prettyList body
    --pretty' (Hash x) = _
    pretty' (Array ar) = list (map pretty ar)

