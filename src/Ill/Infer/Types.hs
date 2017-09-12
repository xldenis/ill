{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Ill.Infer.Types where
import           Control.Monad.Unify

import           Ill.Infer.Monad
import           Ill.Syntax

import qualified Data.HashMap.Strict as H
import           Data.List
import           Data.Maybe
import           Ill.Error
import           Ill.Parser.Lexer    (SourceSpan (..))
import Data.Bifunctor

infer :: Expr SourceSpan -> UnifyT (Type Name) Check (Expr TypedAnn)
infer (a :< Apply l args) = do
  f' <- infer l

  args' <- mapM infer args
  retTy <- fresh

  constraints <- typeOf f' `constrainedUnification` flattenConstraints (foldr tFn retTy (map typeOf args'))
  let retTy' = constrain constraints retTy

  return $ Ann a (Type retTy') :< Apply f' args'
infer (a :< If cond left right) = do
  cond' <- check tBool cond
  left' <- infer left
  right' <- infer right
  constraints <- typeOf left' `constrainedUnification` typeOf right'

  let (cons, _) = unconstrained $ typeOf cond'
      retTy     = constrain (cons ++ constraints) $ typeOf left'

  return $ Ann a (Type $ retTy) :< If cond' left' right'
infer (a :< Case cond branches) = do
  cond' <- infer cond
  retTy <- fresh

  branches' <- forM branches $ \(pattern, expr) -> do
    (dict, pattern') <- inferPat (typeOf cond') pattern

    expr' <- bindNames dict (infer expr)
    typeOf expr' `constrainedUnification` retTy

    return $ (pattern', expr')

  return $ Ann a (Type . typeOf . snd $ last branches') :< Case cond' branches'
infer (a :< Body es) = do
  tys <- bindNames [] $ mapM infer es

  let bodyCons = tys >>= constraints . typeOf
      retTy    = constrain bodyCons (typeOf $ last tys)

  return $ Ann a (Type retTy) :< Body tys
infer (a :< BinOp op l r) = do
  op' <- infer op
  l' <- infer l
  r' <- infer r
  retTy <- fresh

  constraints <- typeOf op' `constrainedUnification` flattenConstraints (foldr tFn retTy [typeOf l', typeOf r'])
  let retTy' = constrain constraints retTy

  return $ Ann a (Type retTy') :< BinOp op' l' r'
infer (a :< Lambda pats expr) = do
  patTys <- replicateM (length pats) fresh

  (patDict, pats') <- inferPats (zip patTys pats)
  expr' <- bindNames patDict (infer expr)

  let retTy = foldr tFn (typeOf expr') patTys
  return $ Ann a (Type retTy) :< Lambda pats' expr'
infer (a :< Assign lnames exps) = do
  varTys <- replicateM (length lnames) fresh
  let bound = zip lnames varTys
  modifyEnv $ \e -> e { names = bound ++ (names e) }

  exps' <- mapM infer exps
  zipWithM ((=?=) . typeOf) exps' varTys

  return $ Ann a (Type tNil) :< Assign lnames exps'
infer (a :< Var nm) = do
  ty <-  lookupVariable nm
  ty' <- freshenFunction ty

  return $ Ann a (Type ty') :< Var nm
infer (a :< Constructor nm) = do
  (_, ty, args) <- lookupConstructor nm

  subs <- mapM (\a -> (,) <$> pure a <*> fresh) args

  let nT = replaceTypeVars subs ty

  return $ Ann a (Type nT) :< Constructor nm
infer (a :< Literal l) = do
  let ty = inferLit l
  return $ Ann a (Type ty) :< Literal l

check :: Type Name -> Expr SourceSpan -> UnifyT (Type Name) Check (Expr TypedAnn)
check expected (a :< If cond l r) = do
  cond' <- check tBool cond
  left' <- check expected l
  right' <- check expected r

  return $ Ann a (Type expected) :< If cond' left' right'
check expected (a :< Var nm) = do
  ty <- lookupVariable nm
  cons <- ty `constrainedUnification` expected
  return $ Ann a (Type $ constrain cons ty) :< Var nm
check expected (a :< Constructor nm) = do
  (_, ty, args) <- lookupConstructor nm

  subs <- mapM (\a -> (,) <$> pure a <*> fresh) args

  let nT = replaceTypeVars subs ty
  nT =?= expected

  return $ Ann a (Type nT) :< Constructor nm
check expected v@(_ :< BinOp _ _ _) = do
  v' <- infer v
  -- internalError $ typeOf v'
  -- internalError $ show expected ++ show (typeOf v')
  cons <- typeOf v' `constrainedUnification` expected

  return $ fmap (modifyTyAnn (constrain cons)) v'
  where

  modifyTyAnn f (Ann src (Type ty)) = Ann src (Type $ f ty)
  modifyTyAnn f t = t

check expected (a :< Body es) = do
  tys <- mapM infer es

  cons <- (typeOf $ last tys) `constrainedUnification` expected

  return $ Ann a (Type . typeOf $ last tys) :< Body tys
check expected (a :< Apply f args) = do
  f' <- infer f
  let (constraints, ty') = unconstrained (typeOf f')
      unwrapped = unwrapN (length args) ty'
      argTys    = init unwrapped
      retTy     = last unwrapped

  args' <- mapM (uncurry check) (zip argTys args)

  return $ Ann a (Type retTy) :< Apply f' args'

check expected ty = error (show ty)


instance Partial (Type a) where
  unknown                    = TUnknown

  isUnknown (TUnknown u) = Just u
  isUnknown _            = Nothing

  unknowns (TAp l r)         = unknowns l ++ unknowns r
  unknowns (Arrow l r)       = unknowns l ++ unknowns r
  unknowns (Constrained ts t) = concatMap unknowns' ts ++ unknowns t
    where unknowns' (nm, ts) = concatMap unknowns ts
  unknowns _                 = []

  ($?) sub (TAp l r)         = TAp (sub $? l) (sub $? r)
  ($?) sub (Arrow l r)       = Arrow (sub $? l) (sub $? r)
  ($?) sub (Constrained ts t) = Constrained (map sub' ts) (sub $? t)
    where sub' (nm, ts) = (nm, map (sub $?) ts)
  ($?) sub t@(TUnknown u)    = fromMaybe t $ H.lookup u (runSubstitution sub)
  ($?) sub other             = other

instance UnificationError (Type Name) MultiError where
  occursCheckFailed t = TypeOccursError t

instance Unifiable Check (Type Name) where
  (=?=) = unifyTypes

unifyTypes (TUnknown u1) (TUnknown u2) | u1 == u2 = return ()
unifyTypes (TUnknown u) t = u =:= t
unifyTypes t (TUnknown u) = u =:= t
unifyTypes (TVar v1) (TVar v2) | v1 == v2 = return ()
unifyTypes (TAp l1 r1) (TAp l2 r2) = do
  l1 `unifyTypes` l2
  r1 `unifyTypes` r2
unifyTypes (Arrow l1 r1) (Arrow l2 r2) = do
  l1 `unifyTypes` l2
  r1 `unifyTypes` r2
unifyTypes c1@(TConstructor cNm1) c2@(TConstructor cNm2) =
  if c1 == c2
  then return ()
  else throwError $ UnificationError c1 c2
unifyTypes t1 t2 = throwError $ UnificationError t1 t2

constrainedUnification :: Type Name -> Type Name -> UnifyT (Type Name) Check [Constraint Name]
constrainedUnification t1 t2 = let
  (cons1, t1') = unconstrained t1
  (cons2, t2') = unconstrained t2

  in t1' =?= t2' >> return (cons1 ++ cons2)

type Alt a = (Patterns a, Expr a)

inferLit (RawString _) = tString
inferLit (EscString _) = tString
inferLit (Integer _ )  = tInteger
inferLit (Double _)    = tDouble

inferPats :: [(Type Name, Pat SourceSpan)] -> UnifyT (Type Name) Check ([(Name, Type Name)], Patterns TypedAnn)
inferPats pats = (first concat . unzip) <$> mapM (uncurry inferPat) pats

inferPat :: Type Name -> Pat SourceSpan -> UnifyT (Type Name) Check ([(Name, Type Name)], Pat TypedAnn)
inferPat ty (a :< PVar n) = do
  f <- fresh
  ty =?= f
  return ([(n, f)], Ann a (Type f) :< PVar n)
inferPat ty (a :< Destructor n pats) = do
  (_, t, _) <- lookupConstructor n
  freshened <- freshenFunction t
  (dict, subPats) <- go pats freshened

  return (dict, Ann a (Type freshened) :< Destructor n subPats)
  where
  go :: [Pat SourceSpan] -> Type Name -> UnifyT (Type Name) Check ([(Name, Type Name)], Patterns TypedAnn)
  go [] ty' = ty =?= ty' *> pure ([], [])
  go (pat : pats) (TAp (TAp f a) b) | f == tArrow = do
    (n1, p1) <- inferPat a pat
    (n2, p2) <- go pats b
    pure (n1 ++ n2, p1 : p2)
  go (pat : pats) (Arrow a b) = do
    (n1, p1) <- inferPat a pat
    (n2, p2) <- go pats b
    pure (n1 ++ n2, p1 : p2)
  go _ _ = throwError $ InternalError "Impossible"
inferPat ty (a :< PLit lit) = do
  let litTy = inferLit lit
  litTy =?= ty
  return ([], Ann a (Type litTy) :< PLit lit)

freshenFunction :: Type Name -> UnifyT (Type Name) Check (Type Name)
freshenFunction ty = do
  let vars = nub $ varsInType ty
  replacements <- forM vars $ \var -> (,) <$> pure var <*> fresh

  return $ replaceTypeVars replacements ty
