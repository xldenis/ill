{-# LANGUAGE DeriveFunctor, FlexibleInstances #-}
module Ill.Syntax.Expression where
import Control.Comonad.Cofree
import Control.Comonad

import Ill.Syntax.Pretty

import Ill.Syntax.Pattern
import Ill.Syntax.Literal

import           Ill.Inference.Class (Infer)
import           Ill.Inference.Type hiding (list)

data Expression a
  = Apply a [a]
  | BinOp a a a
  | Assign [String] [a]
  | Case a [(Pattern, a)]
  | If a a a
  | Lambda [Pattern] a
  | Var String
  | Literal Literal
  | Body [a]
  | Hash [(a, a)]
  | Array [a]
  deriving (Eq, Functor, Show)

type Expr a = Cofree Expression a

instance Pretty (Cofree Expression a) where
  pretty (_ :< f) = pretty' f where
    pretty' (Apply func args) = pretty func <> tupled (map pretty args)
    pretty' (BinOp op l r) = pretty l <+> pretty op <+> pretty r
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

tiExpr :: Infer (Expr a) Type
tiExpr ce as (_ :< Var i) = do
  sc         <- find i as
  (ps :=> t) <- freshInst sc
  return (ps, t)
tiExpr ce as (_ :< Apply f es) = do
  tes <- mapM (tiExpr ce as) es
  (qs,tf) <- tiExpr ce as f
  t       <- newTVar Star
  let (epreds, te) = foldr (\(preds, te) (opreds, to) -> (preds ++ opreds, te `fn` to)) ([], t) tes
  unify tf te
  return (epreds ++ qs, t)
tiExpr ce as (_ :< Body es) = do
  tys <- mapM (tiExpr ce as) es
  return (last tys)
tiExpr ce as (_ :< Literal l) = do
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

  let fTy = foldr1 fn pTy
  return $ (pPred ++ ePred, fTy `fn` tBody)
-- tiExpr ce as (_ :< Assign vars vals) = do
--   tes <- mapM (tiExpr ce as) es

--   let zipped = zip vars tes
--       bleh = for zipped $ \(name, (preds, ty)) ->
--     name :>: Forall [] (preds :=> ty)



tiAlt :: Infer ([Pattern], Expr a) Type
tiAlt ce as (pats, body) = do
  (pPred, pAssum, pTy) <- tiPats pats
  (ePred, tBody) <- tiExpr ce (as ++ pAssum) body

  let fTy = foldr1 fn pTy
  return $ (pPred ++ ePred, fTy `fn` tBody)

-- tiAlts :: Infer [([Pattern], Expr)] Type
-- tiAlts ce as alts = do
--   pInf <- mapM (tiAlt ce as) alts


bleh ce as (_ :< Apply f es) = do
  tes <- mapM (tiExpr ce as) es
  (qs,tf) <- tiExpr ce as f
  t       <- newTVar Star
  let (epreds, te) = foldr (\(opreds, to) (preds, te) -> (preds ++ opreds, te `fn` to)) ([], t) tes
  return (tf, te)
