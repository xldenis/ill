{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
module Ill.Syntax.Expression where
import           Control.Comonad
import           Control.Comonad.Cofree
import Data.Traversable (forM)
import           Ill.Syntax.Pretty

import           Ill.Syntax.Literal
import           Ill.Syntax.Pattern

import           Ill.Inference.Class    (Infer)
import           Ill.Inference.Type     hiding (list)

type Patterns = Pattern

data Expression a
  = Apply a [a]
  | BinOp a a a
  | Assign [String] [a] a
  | Case a [(Patterns, a)]
  | If a a a
  | Lambda [Pattern] a
  | Var String
  | Literal Literal
  | Body [a]
  -- | Hash [(a, a)]
  | Array [a]
  deriving (Eq, Functor, Show)

type Expr a = Cofree Expression a

instance Pretty (Cofree Expression a) where
  pretty (_ :< f) = pretty' f where
    pretty' (Apply func args) = pretty func <> tupled (map pretty args)
    pretty' (BinOp op l r) = pretty l <+> pretty op <+> pretty r
    pretty' (Assign idents exprs body) = cat (punctuate comma (map text idents)) <+> char '=' <+> cat (punctuate comma (map pretty exprs))
      `above` pretty body
    pretty' (Case cond branches) = text "case" <+> pretty cond <+> text "of" `above` vsep (map prettyBranch branches)
      where prettyBranch (pat, branch) = pretty pat <+> text "->" <+> pretty branch
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

tiExpr :: Infer (Expr a) Type
tiExpr _ as (_ :< Var i) = do
  sc         <- find i as
  (ps :=> t) <- freshInst sc
  return (ps, t)
tiExpr ce as (_ :< Apply f es) = do
  tes <- mapM (tiExpr ce as) es
  (qs,tf) <- tiExpr ce as f
  t       <- newTVar Star
  let (epreds, tef) = foldr (\(preds, te) (opreds, to) -> (preds ++ opreds, te `fn` to)) ([], t) tes
  unify tf tef
  return (epreds ++ qs, t)
tiExpr ce as (_ :< Body es) = do
  tys <- mapM (tiExpr ce as) es
  return (last tys)
tiExpr _ _ (_ :< Literal l) = do
  (ps,t) <- tiLit l
  return (ps, t)
tiExpr ce as (_ :< If c l r) = do
  (ps, condTy) <- tiExpr ce as c
  unify condTy tBool

  (lpreds, lTy) <- tiExpr ce as l
  (rpreds, rTy) <- tiExpr ce as r
  unify lTy rTy

  return (ps ++ lpreds ++ rpreds, lTy)
tiExpr ce as (_ :< Lambda pats body) = do
  (pPred, pAssum, pTy) <- tiPats pats
  (ePred, tBody) <- tiExpr ce (as ++ pAssum) body

  let fTy = foldr1 fn $ pTy ++ [tBody]
  return (pPred ++ ePred, fTy)
tiExpr ce as (_ :< Assign vars vals sucs) = do
  tes <- mapM (tiExpr ce as) vals

  let zipped = zip vars tes
      bleh = flip map zipped $ \(name, (preds, ty)) ->
        name :>: Forall [] (preds :=> ty)

  tiExpr ce (as ++ bleh) sucs
tiExpr ce as (_ :< BinOp op l r) = do
  t       <- newTVar Star
  (oPreds, tOp) <- tiExpr ce as op
  (lPreds, tL)  <- tiExpr ce as l
  (rPreds, tR)  <- tiExpr ce as r

  unify tOp $ tL `fn` tR `fn` t

  return (oPreds ++ lPreds ++ rPreds, t)
tiExpr ce as (_ :< Case cond branches) = do
  (cPred, tCond) <- tiExpr ce as cond
  t <- newTVar Star
  preds <- forM branches $ \(pat, body) -> do
    (pPred, pAssum, tPat) <- tiPat pat
    unify tCond tPat
    (ePred, tE) <- tiExpr ce (as ++ pAssum) body
    unify tE t
    return $ pPred ++ ePred

  return (cPred ++ concat preds, t)
tiExpr ce as (_ :< Array _) = undefined

