{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ill.Syntax.Expression where
import           Control.Lens.TH

import           Control.Comonad.Cofree
import           Ill.Syntax.Pretty

import           Ill.Syntax.Literal
import           Ill.Syntax.Pattern

import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable

import Control.Monad (join)

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

instance Bifoldable Expression where
  bifoldMap l r (Case a brs) = (r a) `mappend` foldMap (bifoldMap (foldMap l) r) brs
  bifoldMap l r val = bifoldMapDefault l r val

instance Bitraversable Expression where
  bitraverse l r (Case a brs) = Case <$> (r a) <*> traverse helper brs
    where helper (pat, exp) = (,) <$> traverse l pat <*> (r exp)
  bitraverse l r (Apply a as) = Apply <$> r a <*> (traverse r as)
  bitraverse l r (BinOp o a b) = BinOp <$> (r o) <*> (r a) <*> (r b)
  bitraverse l r (Assign s as) = Assign s <$> (traverse r as)
  bitraverse l r (If c a b) = If <$> (r c) <*> (r a) <*> (r b)
  bitraverse l r (Lambda ps b) = Lambda <$> (traverse (traverse l) ps) <*> (r b)
  bitraverse l r (Var s) = pure $ Var s
  bitraverse l r (Constructor s) = pure $ Constructor s
  bitraverse l r (Literal a) = pure $ Literal a
  bitraverse l r (Body bs) = Body <$> (traverse r bs)
  bitraverse l r (Array as) = Array <$> (traverse r as)

hoistBiCofree :: forall t m a g. (Traversable t, Monad m) => (forall x . t x -> m (g x)) -> Cofree t a -> m (Cofree g a)
hoistBiCofree f (x :< y) = (x :<) <$> (f =<< hoistBiCofree f `traverse` y)

instance Pretty1 (Expression a) where
  liftPretty pretty' (Apply func args) = pretty' func <> tupled (map pretty' args)
  liftPretty pretty' (BinOp op l r) = pretty' l <+> pretty' op <+> pretty' r
  liftPretty pretty' (Assign idents exprs) = cat (punctuate comma (map pretty idents)) <+> pretty '=' <+> cat (punctuate comma (map pretty' exprs))
  liftPretty pretty' (Case cond branches) = nest 2 (vsep'
    [ pretty "case" <+> pretty' cond <+> pretty "of"
    , (vsep (map prettyBranch branches))
    ]) <> hardline <> pretty "end"

    where prettyBranch (pat, branch) = pretty "when" <+> pretty pat <+> pretty ":" <+> pretty' branch
  liftPretty pretty' (If cond left right) = vsep
    [ pretty "if" <+> pretty' cond <+> pretty "then"
    , nest 2 (pretty' left)
    , pretty "else"
    , nest 2 (pretty' right)
    , pretty "end"
    ]
  liftPretty pretty' (Lambda args body) = nest 2 (pretty "fn" <+> tupled (map pretty args) <+> pretty "=" `above` (pretty' body) ) `above` pretty "end"
  liftPretty pretty' (Var v) = pretty v
  liftPretty pretty' (Constructor c) = pretty c
  liftPretty pretty' (Literal l) = pretty l
  liftPretty pretty' (Body body) = vsep $ (map pretty' body)
  --liftPretty pretty' (Hash x) = _
  liftPretty pretty' (Array ar) = list (map pretty' ar)

