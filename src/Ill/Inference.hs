module Ill.Inference (module X, module Ill.Inference) where

import Ill.Inference.Type as X
import Ill.Inference.Class as X


import Control.Monad (liftM, zipWithM, ap)
import Data.List ((\\), union, intersect, partition)

import qualified Ill.Syntax as ST

typeFromSyntax :: ST.Type Id -> Type
typeFromSyntax (ST.TVar t) = TVar (Tyvar t Star)
typeFromSyntax (ST.Arrow a b) = fn (typeFromSyntax a) (typeFromSyntax b)
typeFromSyntax (ST.Constructor n args) = TCon (Tycon n (kfn $ length args))
  where kfn 0 = Star
        kfn n = KFun Star (kfn $ n-1)

type Alt = ([ST.Pattern], ST.Expr ())

tiAlt                :: Infer Alt Type
tiAlt ce as (pats, e) = do (ps, as', ts) <- ST.tiPats pats
                           (qs,t)  <- ST.tiExpr ce (as'++as) e
                           return (ps++qs, foldr fn t ts)

tiAlts             :: ClassEnv -> [Assump] -> [Alt] -> Type -> TI [Pred]
tiAlts ce as alts t = do psts <- mapM (tiAlt ce as) alts
                         mapM_ (unify t . snd) psts
                         return (concatMap fst psts)

split :: Monad m => ClassEnv -> [Tyvar] -> [Tyvar] -> [Pred]
                      -> m ([Pred], [Pred])
split ce fs gs ps = do ps' <- reduce ce ps
                       let (ds, rs) = partition (all (`elem` fs) . free) ps'
                       rs' <- defaultedPreds ce (fs++gs) rs
                       return (ds, rs \\ rs')

type Ambiguity       = (Tyvar, [Pred])

ambiguities         :: ClassEnv -> [Tyvar] -> [Pred] -> [Ambiguity]
ambiguities ce vs ps = [ (v, filter (elem v . free) ps) | v <- free ps \\ vs ]

numClasses :: [Id]
numClasses  = ["Num", "Integral", "Floating", "Fractional", "Real", "RealFloat", "RealFrac"]

stdClasses :: [Id]
stdClasses  = ["Eq", "Ord", "Show", "Read", "Bounded", "Enum", "Ix",
               "Functor", "Monad", "MonadPlus"] ++ numClasses

candidates           :: ClassEnv -> Ambiguity -> [Type]
candidates ce (v, qs) = [ t' | let is = [ i | IsIn i t <- qs ]
                                   ts = [ t | IsIn i t <- qs ],
                               all (TVar v==) ts,
                               any (`elem` numClasses) is,
                               all (`elem` stdClasses) is,
                               t' <- defaults ce,
                               all (entail ce []) [ IsIn i t' | i <- is ] ]

withDefaults :: Monad m => ([Ambiguity] -> [Type] -> a)
                  -> ClassEnv -> [Tyvar] -> [Pred] -> m a
withDefaults f ce vs ps
    | any null tss  = fail "cannot resolve ambiguity"
    | otherwise     = return (f vps (map head tss))
      where vps = ambiguities ce vs ps
            tss = map (candidates ce) vps

defaultedPreds :: Monad m => ClassEnv -> [Tyvar] -> [Pred] -> m [Pred]
defaultedPreds  = withDefaults (\vps ts -> concatMap snd vps)

defaultSubst   :: Monad m => ClassEnv -> [Tyvar] -> [Pred] -> m Substitution
defaultSubst    = withDefaults (\vps ts -> zip (map fst vps) ts)

type Expl = (Id, Scheme, [Alt])

tiExpl :: ClassEnv -> [Assump] -> Expl -> TI [Pred]
tiExpl ce as (i, sc, alts)
        = do (qs :=> t) <- freshInst sc
             ps         <- tiAlts ce as alts t
             s          <- getSubst
             let qs'     = apply s qs
                 t'      = apply s t
                 fs      = free (apply s as)
                 gs      = free t' \\ fs
                 sc'     = quantify gs (qs':=>t')
                 ps'     = filter (not . entail ce qs') (apply s ps)
             (ds,rs)    <- split ce fs gs ps'
             if sc /= sc' then
                 fail "signature too general"
               else if not (null rs) then
                 fail "context too weak"
               else
                 return ds

type Impl   = (Id, [Alt])

restricted   :: [Impl] -> Bool
restricted = any simple
 where simple (i,alts) = any (null . fst) alts

tiImpls         :: Infer [Impl] [Assump]
tiImpls ce as bs = do ts <- mapM (\_ -> newTVar Star) bs
                      let is    = map fst bs
                          scs   = map toScheme ts
                          as'   = zipWith (:>:) is scs ++ as
                          altss = map snd bs
                      pss <- Control.Monad.zipWithM (tiAlts ce as') altss ts
                      s   <- getSubst
                      let ps'     = apply s (concat pss)
                          ts'     = apply s ts
                          fs      = free (apply s as)
                          vss     = map free ts'
                          gs      = foldr1 union vss \\ fs
                      (ds,rs) <- split ce fs (foldr1 intersect vss) ps'
                      if restricted bs then
                          let gs'  = gs \\ free rs
                              scs' = map (quantify gs' . ([]:=>)) ts'
                          in return (ds++rs, zipWith (:>:) is scs')
                        else
                          let scs' = map (quantify gs . (rs:=>)) ts'
                          in return (ds, zipWith (:>:) is scs')

type BindGroup  = ([Expl], [[Impl]])

tiBindGroup :: Infer BindGroup [Assump]
tiBindGroup ce as (es,iss) =
  do let as' = [ v:>:sc | (v,sc,alts) <- es ]
     (ps, as'') <- tiSeq tiImpls ce (as'++as) iss
     qss        <- mapM (tiExpl ce (as''++as'++as)) es
     return (ps++concat qss, as''++as')

tiSeq                  :: Infer bg [Assump] -> Infer [bg] [Assump]
tiSeq ti ce as []       = return ([],[])
tiSeq ti ce as (bs:bss) = do (ps,as')  <- ti ce as bs
                             (qs,as'') <- tiSeq ti ce (as'++as) bss
                             return (ps++qs, as''++as')

type Program = [BindGroup]

tiProgram :: ClassEnv -> [Assump] -> Program -> [Assump]
tiProgram ce as bgs = runTI $
                      do (ps, as') <- tiSeq tiBindGroup ce as bgs
                         s         <- getSubst
                         rs        <- reduce ce (apply s ps)
                         s'        <- defaultSubst ce [] rs
                         return (apply (s'@@s) as')
