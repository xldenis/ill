{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Ill.Infer.Types
( infer
, check
, constrainedUnification
, inferPats
) where

import           Control.Monad.Unify
import           Control.Comonad (extract)
import           Control.Monad.State (get)

import qualified Data.HashMap.Strict as H
import           Data.List
import           Data.Maybe
import           Data.Bifunctor

import           Ill.Infer.Monad
import           Ill.Syntax hiding (typeOf)
import           Ill.Error
import           Ill.Parser.Lexer    (SourceSpan (..))

typeOf :: Functor f => Cofree f TypedAnn -> (Type Name)
typeOf c =  fromMaybe (polyTy $ fromTy c) (instTy $ fromTy c)
  where fromTy = ty . extract

infer :: Expr SourceSpan -> UnifyT (Type Name) Check (Expr TypedAnn)
infer exp = rethrow (ErrorInExpression exp) (infer' exp)

infer' :: Expr SourceSpan -> UnifyT (Type Name) Check (Expr TypedAnn)
infer' (a :< Apply l args) = do
  retTy <- fresh
  f' <- infer l

  args' <- mapM infer args
  constraints <- typeOf f' `constrainedUnification` flattenConstraints (foldr tFn retTy $ map typeOf args')
  let retTy' = constrain constraints retTy

  return $ Ann a retTy' :< Apply f' args'
infer' (a :< If cond left right) = do
  cond' <- check tBool cond
  left' <- infer left
  right' <- infer right
  constraints <- typeOf left' `constrainedUnification` typeOf right'

  let (cons, _) = unconstrained $ typeOf cond'
      retTy     = constrain (cons ++ constraints) $ typeOf left'

  return $ Ann a retTy :< If cond' left' right'
infer' (a :< Case cond branches) = do
  cond' <- infer cond
  retTy <- fresh

  branches' <- forM branches $ \(pattern, expr) -> do
    (dict, pattern') <- inferPat (typeOf cond') pattern

    expr' <- bindNames dict (infer expr)
    typeOf expr' `constrainedUnification` retTy

    return $ (pattern', expr')

  return $ Ann a (typeOf . snd $ last branches') :< Case cond' branches'
infer' (a :< Body es) = do
  tys <- bindNames [] $ mapM infer es

  let bodyCons = tys >>= constraints . typeOf
      retTy    = constrain bodyCons (typeOf $ last tys)

  return $ Ann a retTy :< Body tys
infer' (a :< BinOp op l r) = do
  op' <- infer op
  l' <- infer l
  r' <- infer r
  retTy <- fresh

  constraints <- typeOf op' `constrainedUnification` flattenConstraints (foldr tFn retTy [typeOf l', typeOf r'])
  let retTy' = constrain constraints retTy

  return $ Ann a retTy :< BinOp op' l' r'
infer' (a :< Lambda pats expr) = do
  patTys <- replicateM (length pats) fresh

  (patDict, pats') <- inferPats (zip patTys pats)
  expr' <- bindNames patDict (infer expr)

  let retTy = foldr tFn (typeOf expr') patTys
  return $ Ann a retTy :< Lambda pats' expr'
infer' (a :< Assign lnames exps) = do
  varTys <- replicateM (length lnames) fresh
  let bound = zip lnames varTys
  addNames bound

  exps' <- mapM infer exps
  zipWithM ((=?=) . typeOf) exps' varTys

  return $ Ann a tNil :< Assign lnames exps'
infer' (a :< Var nm) = do
  ty <-  lookupVariable nm
  ty' <- instantiate ty

  return $ TyAnn (Just a) (Type ty $ Just ty') :< Var nm
infer' (a :< Constructor nm) = do
  ConstructorEntry { consType = ty, consTyVars = args } <- lookupConstructor nm
  ty' <- instantiate ty

  return $ TyAnn (Just a) (Type ty $ Just ty') :< Constructor nm
infer' (a :< Literal l) = do
  let ty = litType l
  return $ Ann a ty :< Literal l

check :: Type Name -> Expr SourceSpan -> UnifyT (Type Name) Check (Expr TypedAnn)
check ty exp = rethrow (ErrorInExpression exp) $ check' ty exp

check' :: Type Name -> Expr SourceSpan -> UnifyT (Type Name) Check (Expr TypedAnn)
check' expected (a :< If cond l r) = do
  cond' <- check tBool cond
  left' <- check expected l
  right' <- check' expected r

  return $ Ann a expected :< If cond' left' right'
check' expected (a :< Var nm) = do
  ty <- lookupVariable nm
  ty' <- instantiate ty

  cons <- ty' `constrainedUnification` expected

  return $ TyAnn (Just a) (Type ty $ Just $ constrain cons ty') :< Var nm
check' expected (a :< Constructor nm) = do
  ConstructorEntry { consType = ty, consTyVars = args } <- lookupConstructor nm
  ty' <- instantiate ty
  ty' =?= expected

  return $ TyAnn (Just a) (Type ty $ Just ty') :< Constructor nm
check' expected v@(_ :< BinOp _ _ _) = do
  v' <- infer v
  cons <- typeOf v' `constrainedUnification` expected
  return $ fmap (modifyTyAnn (constrain cons)) v'
  where

  modifyTyAnn f (TyAnn src (Type ty m)) = TyAnn src (Type (f ty) m)
  modifyTyAnn f t = t
check' expected (a :< Body [e]) = do
  ty <- check expected e
  return $ Ann a (typeOf ty) :< Body [ty]
check' expected (a :< Body es) = do
  tys <- mapM infer es

  let totalConstraints = nub $ concatMap (constraints . typeOf) tys

  cons <- (typeOf $ last tys) `constrainedUnification` expected

  return $ Ann a (constrain totalConstraints $ typeOf $ last tys) :< Body tys
check' expected (a :< Apply f args) = do
  f' <- infer f

  subst <- unifyCurrentSubstitution <$> UnifyT get
  let fTy' = (subst $?) (typeOf f')

  let (constraints, ty') = unconstrained ( fTy')
      unwrapped = unwrapN (length args) ty'
      argTys    = init unwrapped
      retTy     = last unwrapped

  args' <- mapM (uncurry check) (zip argTys args)

  retTy =?= expected

  return $ Ann a (constrain constraints retTy) :< Apply f' args'
check' expected (a :< Literal lit) = do
  let ty = litType lit
  ty =?= expected
  return $ Ann a ty :< Literal lit
check' expected l@(a :< Lambda pats exp) = do
  l' <- infer l

  expected =?= typeOf l'

  return l'
check' expected c@(a :< Case scrut pats) = do
  c' <- infer c

  typeOf c' =?= expected

  return c'
check' expected ty = error (show $ pretty ty)


instance Partial (Type a) where
  unknown                    = TUnknown

  isUnknown (TUnknown u) = Just u
  isUnknown _            = Nothing

  unknowns (TAp l r)         = unknowns l ++ unknowns r
  unknowns (Arrow l r)       = unknowns l ++ unknowns r
  unknowns (Constrained ts t) = concatMap unknowns' ts ++ unknowns t
    where unknowns' (nm, ts) = concatMap unknowns ts
  unknowns (TUnknown u)      = [u]
  unknowns (Forall _ ty)     = unknowns ty
  unknowns _                 = []

  ($?) sub (TAp l r)         = TAp (sub $? l) (sub $? r)
  ($?) sub (Arrow l r)       = Arrow (sub $? l) (sub $? r)
  ($?) sub (Constrained ts t) = Constrained (map sub' ts) (sub $? t)
    where sub' (nm, ts) = (nm, map (sub $?) ts)
  ($?) sub t@(TUnknown u)    = fromMaybe t $ H.lookup u (runSubstitution sub)
  ($?) sub (Forall vars ty)  = Forall vars $ sub $? ty
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
unifyTypes f@(Forall{}) ty = do
  f' <- instantiate f
  f' `unifyTypes` ty
unifyTypes ty t@(Forall{}) = unifyTypes t ty
unifyTypes t1 t2 = throwError $ UnificationError t1 t2

constrainedUnification :: Type Name -> Type Name -> UnifyT (Type Name) Check [Constraint Name]
constrainedUnification t1 t2 = let
  (cons1, t1') = unconstrained t1
  (cons2, t2') = unconstrained t2
  in t1' =?= t2' >> return (cons1 ++ cons2)

type Alt a = (Patterns a, Expr a)

inferPats :: [(Type Name, Pat SourceSpan)] -> UnifyT (Type Name) Check ([(Name, Type Name)], Patterns TypedAnn)
inferPats pats = (first concat . unzip) <$> mapM (uncurry inferPat) pats

inferPat :: Type Name -> Pat SourceSpan -> UnifyT (Type Name) Check ([(Name, Type Name)], Pat TypedAnn)
inferPat ty pat = rethrow (ErrorInPattern pat) (inferPat' ty pat)

inferPat' :: Type Name -> Pat SourceSpan -> UnifyT (Type Name) Check ([(Name, Type Name)], Pat TypedAnn)
inferPat' ty (a :< PVar n) = do
  f <- fresh
  ty =?= f
  return ([(n, f)], Ann a f :< PVar n)
inferPat' ty (a :< Destructor n pats) = do
  ConstructorEntry { consType = t } <- lookupConstructor n
  freshened <- instantiate t
  (dict, subPats) <- go pats freshened

  return (dict, Ann a freshened :< Destructor n subPats)
  where
  go :: [Pat SourceSpan] -> Type Name -> UnifyT (Type Name) Check ([(Name, Type Name)], Patterns TypedAnn)
  go [] ty' = ty =?= ty' *> pure ([], [])
  go (pat : pats) (TAp (TAp f a) b) | f == tArrow = rethrow (ErrorInPattern pat) $ do
    (n1, p1) <- inferPat a pat
    (n2, p2) <- go pats b
    pure (n1 ++ n2, p1 : p2)
  go (pat : pats) (Arrow a b) = rethrow (ErrorInPattern pat) $ do
    (n1, p1) <- inferPat a pat
    (n2, p2) <- go pats b
    pure (n1 ++ n2, p1 : p2)
  go p t = throwError $ InternalError $ (show t) ++ show p
inferPat' ty (a :< PLit lit) = do
  let litTy = litType lit
  litTy =?= ty
  return ([], Ann a litTy :< PLit lit)
inferPat' ty (a :< Wildcard) = do
  return ([], Ann a ty :< Wildcard)
inferPat' ty pat = prettyInternal (ty, pat)

instantiate :: Type Name -> UnifyT (Type Name) Check (Type Name)
instantiate (Forall vars ty) = do
  replacements <- forM vars $ \var -> (,) <$> pure var <*> fresh
  return $ replaceTypeVars replacements ty
instantiate t = pure t
