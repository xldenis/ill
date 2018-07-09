{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables #-}
module Thrill.Infer.Types
( infer
, check
, constrainedUnification
, (=??=)
, inferPats
) where

import           Thrill.Prelude

import           Control.Monad.Unify
import           Control.Comonad (extract)
import           Control.Monad.State (get)
import           Control.Monad.Writer

import qualified Data.HashMap.Strict as H
import           Data.Bifunctor

import           Thrill.Infer.Monad
import           Thrill.Syntax hiding (typeOf)
import           Thrill.Parser.Lexer    (SourceSpan (..))

import Debug.Trace

typeOf :: Functor f => Cofree f TypedAnn -> (Type QualifiedName)
typeOf c =  fromMaybe (polyTy $ fromTy c) (instTy $ fromTy c)
  where fromTy = ty . extract

infer :: Expr' QualifiedName SourceSpan -> WriterT [Constraint QualifiedName] (UnifyT (Type QualifiedName) Check) (Expr' QualifiedName TypedAnn)
infer exp = rethrow (ErrorInExpression exp) (infer' exp)

infer' :: Expr' QualifiedName SourceSpan -> WriterT [Constraint QualifiedName] (UnifyT (Type QualifiedName) Check) (Expr' QualifiedName TypedAnn)
infer' (a :< Apply l args) = do
  retTy <- lift fresh
  f' <- infer l

  args' <- mapM infer args

  fullyInstantiated <- lift $ instantiate (typeOf f')
  fullyInstantiated =??= flattenConstraints (foldr tFn retTy $ map typeOf args')

  return $ Ann a retTy :< Apply f' args'
infer' (a :< If cond left right) = do
  cond' <- check tBool cond
  left' <- infer left
  right' <- infer right
  typeOf left' =??= typeOf right'

  return $ Ann a (typeOf left') :< If cond' left' right'
infer' (a :< Case cond branches) = do
  cond' <- infer cond
  retTy <- lift fresh

  branches' <- forM branches $ \(pattern, expr) -> do
    (dict, pattern') <- lift $ inferPat (typeOf cond') pattern

    expr' <- bindNames dict (infer expr)
    typeOf expr' =??= retTy

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
  retTy <- lift fresh

  typeOf op' =??= flattenConstraints (foldr tFn retTy [typeOf l', typeOf r'])

  return $ Ann a retTy :< BinOp op' l' r'
infer' (a :< Lambda pats expr) = do
  patTys <- replicateM (length pats) (lift fresh)

  (patDict, pats') <- lift $ inferPats (zip patTys pats)
  expr' <- bindNames patDict (infer expr)

  let retTy = flattenConstraints $ foldr tFn (typeOf expr') patTys
  return $ Ann a retTy :< Lambda pats' expr'
infer' (a :< Assign lnames exps) = do
  varTys <- replicateM (length lnames) (lift fresh)
  let bound = zip lnames varTys
  addNames bound

  exps' <- mapM infer exps
  zipWithM_ (constrainedUnification . typeOf) exps' varTys

  return $ Ann a tNil :< Assign lnames exps'
infer' (a :< Var nm) = do
  ty <-  lookupVariable nm
  ty' <- lift $ instantiate ty

  return $ TyAnn (Just a) (Type ty $ Just ty') :< Var nm
infer' (a :< Constructor nm) = do
  ConstructorEntry { consType = ty, consTyVars = args } <- lookupConstructor nm
  ty' <- lift $ instantiate ty

  return $ TyAnn (Just a) (Type ty $ Just ty') :< Constructor nm
infer' (a :< Literal l) = do
  let ty = litType l
  return $ Ann a ty :< Literal l

check :: Type QualifiedName -> Expr' QualifiedName SourceSpan -> WriterT [Constraint QualifiedName] (UnifyT (Type QualifiedName) Check) (Expr' QualifiedName TypedAnn)
check ty exp = rethrow (ErrorInExpression exp) $ check' ty exp

check' :: Type QualifiedName -> Expr' QualifiedName SourceSpan -> WriterT [Constraint QualifiedName] (UnifyT (Type QualifiedName) Check) (Expr' QualifiedName TypedAnn)
check' expected (a :< If cond l r) = do
  cond' <- check tBool cond
  left' <- check expected l
  right' <- check' expected r

  return $ Ann a expected :< If cond' left' right'
check' expected (a :< Var nm) = do
  ty <- lookupVariable nm
  ty' <- lift $ instantiate ty

  ty' =??= expected

  return $ TyAnn (Just a) (Type ty $ Just ty') :< Var nm
check' expected (a :< Constructor nm) = do
  ConstructorEntry { consType = ty, consTyVars = args } <- lookupConstructor nm
  ty' <- lift $ instantiate ty
  ty' =??= expected

  return $ TyAnn (Just a) (Type ty $ Just ty') :< Constructor nm
check' expected v@(_ :< BinOp _ _ _) = do
  v' <- infer v

  typeOf v' =??= expected

  return v'
  where

  modifyTyAnn f (TyAnn src (Type ty m)) = TyAnn src (Type (f ty) m)
  modifyTyAnn f t = t
check' expected (a :< Body [e]) = do
  ty <- check expected e
  return $ Ann a (typeOf ty) :< Body [ty]
check' expected (a :< Body es) = do
  tys <- mapM infer es

  let totalConstraints = nub $ concatMap (constraints . typeOf) tys

  cons <- (typeOf $ last tys) =??= expected
  tell totalConstraints

  return $ Ann a (typeOf $ last tys) :< Body tys
check' expected (a :< Apply f args) = do
  f' <- infer f
  fTy <- lift $ instantiate (typeOf f')

  subst <- lift $ unifyCurrentSubstitution <$> UnifyT get
  let fTy' = subst $? fTy

  let (conses, ty')  = unconstrained fTy'
      unwrapped = unwrapN (length args) (ty' :: Type QualifiedName)
      argTys    = init unwrapped
      retTy     = last unwrapped

  args' <- mapM (uncurry check) (zip argTys args)

  retTy =??= expected
  tell conses
  return $ Ann a retTy :< Apply f' args'
check' expected (a :< Literal lit) = do
  let ty = litType lit
  ty =??= expected
  return $ Ann a ty :< Literal lit
check' expected l@(a :< Lambda pats exp) = do
  l' <- infer l

  expected =??= typeOf l'

  return l'
check' expected c@(a :< Case scrut pats) = do
  c' <- infer c

  typeOf c' =??= expected

  return c'
check' expected ty = error (show $ pretty ty)


instance Partial (Type a) where
  unknown                    = TUnknown

  isUnknown (TUnknown u) = Just u
  isUnknown _            = Nothing

  unknowns (TAp l r)         = unknowns l ++ unknowns r
  unknowns (Arrow l r)       = unknowns l ++ unknowns r
  unknowns (Constrained ts t) = concatMap unknowns' ts ++ unknowns t
    where unknowns' (nm, ts) = unknowns ts
  unknowns (TUnknown u)      = [u]
  unknowns (Forall _ ty)     = unknowns ty
  unknowns _                 = []

  ($?) sub (TAp l r)         = TAp (sub $? l) (sub $? r)
  ($?) sub (Arrow l r)       = Arrow (sub $? l) (sub $? r)
  ($?) sub (Constrained ts t) = Constrained (map (fmap (sub $?)) ts) (sub $? t)
  ($?) sub t@(TUnknown u)    = fromMaybe t $ H.lookup u (runSubstitution sub)
  ($?) sub (Forall vars ty)  = Forall vars $ sub $? ty
  ($?) sub other             = other

instance UnificationError (Type QualifiedName) CheckError where
  occursCheckFailed t = TypeOccursError t

instance Unifiable Check (Type QualifiedName) where
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

constrainedUnification :: Type QualifiedName -> Type QualifiedName -> WriterT [Constraint QualifiedName] (UnifyT (Type QualifiedName) Check) ()
constrainedUnification t1 t2 = do
  let
    (cons1, t1') = unconstrained t1
    (cons2, t2') = unconstrained t2

  tell (cons1 ++ cons2)

  lift $ t1' =?= t2' >> return ()

a =??= b = constrainedUnification a b

type Alt a = (Patterns QualifiedName a, Expr a)

inferPats :: [(Type QualifiedName, Pat' QualifiedName SourceSpan)] -> UnifyT (Type QualifiedName) Check ([(QualifiedName, Type QualifiedName)], Patterns QualifiedName TypedAnn)
inferPats pats = (first concat . unzip) <$> mapM (uncurry inferPat) pats

inferPat :: Type QualifiedName -> Pat' QualifiedName SourceSpan -> UnifyT (Type QualifiedName) Check ([(QualifiedName, Type QualifiedName)], Pat' QualifiedName TypedAnn)
inferPat ty pat = rethrow (ErrorInPattern pat) (inferPat' ty pat)

inferPat' :: Type QualifiedName -> Pat' QualifiedName SourceSpan -> UnifyT (Type QualifiedName) Check ([(QualifiedName, Type QualifiedName)], Pat' QualifiedName TypedAnn)
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
  go :: [Pat' QualifiedName SourceSpan] -> Type QualifiedName -> UnifyT (Type QualifiedName) Check ([(QualifiedName, Type QualifiedName)], Patterns QualifiedName TypedAnn)
  go [] ty' = ty =?= ty' *> pure ([], [])
  go (pat : pats) (TAp (TAp f a@ArrowConstructor) b) = rethrow (ErrorInPattern pat) $ do
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

instantiate :: Type QualifiedName -> UnifyT (Type QualifiedName) Check (Type QualifiedName)
instantiate (Forall vars ty) = do
  replacements <- forM vars $ \var -> (,) <$> pure var <*> fresh
  return $ replaceTypeVars replacements ty
instantiate (Constrained cs ty) = Constrained cs <$> instantiate ty
instantiate t = pure t
