{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Ill.Infer where

import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Unify
import qualified Data.HashMap.Strict  as H
import           Data.Map             as M (union)
import           Data.Maybe
import           Ill.Desugar
import           Ill.Infer.Monad
import           Ill.Parser.Lexer     (SourceSpan)
import           Ill.Syntax
import           Ill.Infer.Kind

import Data.List (unfoldr, uncons)

import Data.Coerce

type RawDecl = Decl SourceSpan

typeCheck :: [BindingGroup SourceSpan] -> Check [BindingGroup TypedAnn]
typeCheck bgs = mapM go bgs
  where
  go :: BindingGroup SourceSpan -> Check (BindingGroup TypedAnn)
  go (ValueBG ds)                  = do
    t <- fmap appSubs . liftUnify $ do
      (ut, _, dict, untypedDict) <- typeDictionary ds
      forM ut $ \e -> typeForBindingGroupEl e untypedDict
    forM t $ \v -> addValue (valueName v) (typeOf v)
    return $ ValueBG t
    where valueName (_ :< Value n _) = n
          appSubs (ts, sub) = map (\t -> nestedFmap (\a -> a { ty = fmapTy (sub $?) (ty a) }) t) ts
          fmapTy f (Type t) = Type (f t)
          fmapTy f t = t

  go d@(DataBG  ds)                = do
    let dataDecls = map (\(_ :< Data nm args cons) -> (nm, args, map consPair cons)) ds

    kinds <- kindsOfAll [] (map (\(nm, param, cons) -> (nm, param, concatMap (snd) cons)) dataDecls)

    forM_ (zip dataDecls kinds) $ \((name, args, ctors), ctorKind) -> do
      addDataType name args ctors ctorKind

    ds' <- forM (zip ds kinds) $ \(span :< d, k) -> do
      d' <- addAnn d
      return $ (Ann span (Kind k)) :< d'
    return (DataBG ds')
    where
    addAnn :: Declaration (SourceSpan) (Decl SourceSpan) -> Check (Declaration TypedAnn (Decl TypedAnn))
    addAnn (Data n vars cons) = return $ Data n vars cons
    addAnn n             = throwError "internal error"

    consPair :: Type Name -> (Name, [Type Name])
    consPair   = (\(TConstructor n, b) -> (n,b)) . fromJust . uncons . reverse . unfoldCons
    unfoldCons (TAp f a) = a : unfoldCons f
    unfoldCons a = [a]

  go (OtherBG (_ :< TypeSynonym _ _ _)) = throwError "oops"
  go (OtherBG (a :< (Import q m n al))) = return $ OtherBG $ Ann a (Type tBool) :< (Import q m n al)
  go (OtherBG (_ :< TraitDecl _ _))     = throwError "oops"
  go (OtherBG (_ :< TraitImpl _ _))     = throwError "oops"
  go (OtherBG (_))                 = throwError "oops"

addValue :: Ident -> Type Name -> Check ()
addValue name ty = do
  env <- env <$> get
  let env' = env { names = (name, ty) : names env  }
  modify $ \s -> s { env = env' }

addDataType :: Name -> [Name] -> [(Name, [Type Name])] -> Kind -> Check ()
addDataType name args dctors ctorKind = do
  env <- env <$> get
  let value = (ctorKind)
  let env' = env { types = (name, ctorKind) : (types env) }
  forM_ dctors $ \(dctor, tys) ->
    addDataConstructor name args dctor tys

addDataConstructor :: Name -> [Name] -> Name -> [Type Name] -> Check ()
addDataConstructor name args dctor tys = do
  env <- env <$> get
  let retTy = foldl TAp (TConstructor name) (map TVar args)
      consTy = foldr Arrow retTy tys
      fields = args
  putEnv $ env { constructors = (dctor, (name, consTy, fields)) : (constructors env)}
  return ()

data TypedAnn = Ann { span :: SourceSpan, ty :: TypeAnn }
  deriving (Show, Eq)

data TypeAnn
  = Type (Type Name)
  | Kind Kind
  | None
  deriving (Show, Eq)

infer :: Expr SourceSpan -> UnifyT (Type Name) Check (Expr TypedAnn)
infer (a :< Apply l args) = do
  f' <- infer l
  args' <- mapM infer args

  retTy <- fresh
  typeOf f' =?= foldr Arrow retTy (map typeOf args')

  return $ Ann a (Type retTy) :< Apply f' args'
infer (a :< If cond left right) = do
  cond' <- check cond tBool
  left' <- infer left
  right' <- infer right

  typeOf left' =?= typeOf right'

  return $ Ann a (Type $ typeOf left') :< If cond' left' right'
infer (a :< Body es) = do
  tys <- mapM infer es

  return $ Ann a (Type . typeOf $ last tys) :< Body tys
infer (a :< BinOp op l r) = do
  op' <- infer op
  l' <- infer l
  r' <- infer r
  tRet <- fresh

  typeOf op' =?= (typeOf l' `TAp` (typeOf r' `TAp` tRet))

  return $ Ann a (Type tRet) :< BinOp op' l' r'

infer (a :< Var nm) = do
  ty <- lookupVariable nm

  return $ Ann a (Type ty) :< Var nm
infer (a :< Constructor nm) = do
  (_, ty, args) <- lookupConstructor nm

  subs <- mapM (\a -> (,) <$> pure a <*> fresh) args

  let nT = replaceTypeVars subs ty

  return $ Ann a (Type nT) :< Constructor nm
infer (a :< Literal l) = do
  let ty = inferLit l
  return $ Ann a (Type ty) :< Literal l
  where
  inferLit (RawString _) = tString
  inferLit (EscString _) = tString
  inferLit (Integer _ )  = tInteger
  inferLit (Double _)    = tDouble

typeOf :: Cofree a TypedAnn -> (Type Name)
typeOf (ann :< _) = fromType $ ty ann
  where fromType (Type t) = t

check :: Expr SourceSpan -> Type Name -> UnifyT (Type Name) Check (Expr TypedAnn)
check = undefined

instance Partial (Type a) where
  unknown                    = TUnknown

  isUnknown (TUnknown u) = Just u
  isUnknown _            = Nothing

  unknowns (TAp l r)         = unknowns l ++ unknowns r
  unknowns (Arrow l r)       = unknowns l ++ unknowns r
  unknowns (Trait _ r)       = unknowns r
  unknowns (Constraint ts t) = concatMap unknowns ts ++ unknowns t
  unknowns _                 = []

  ($?) sub (TAp l r)         = TAp (sub $? l) (sub $? r)
  ($?) sub (Arrow l r)       = Arrow (sub $? l) (sub $? r)
  ($?) sub (Trait n t)       = Trait n (sub $? t)
  ($?) sub (Constraint ts t) = Constraint (map (sub $?) ts) (sub $? t)
  ($?) sub t@(TUnknown u)    = fromMaybe t $ H.lookup u (runSubstitution sub)
  ($?) sub other             = other

instance UnificationError (Type Name) String where
  occursCheckFailed t = "occurs check failed: " ++ show t

instance Unifiable Check (Type Name) where
  (=?=) = unifyTypes

unifyTypes :: (Type Name) -> (Type Name) -> UnifyT (Type Name) Check ()
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
unifyTypes (TConstructor c1) (TConstructor c2) =
  if c1 == c2
  then return ()
  else throwError "types do not unify"
-- unifyTypes t1@(TConstructor c1) t2@(TVar v1) = t1 =:= t2
-- unifyTypes t1@(TVar v1) t2@(TConstructor c1) = t1 =:= t2
unifyTypes (Constraint ts1 t1) t2 = throwError "constrained type unification"
unifyTypes t1 (Constraint ts2 t2) = throwError "constrained type unification"
unifyTypes t1 t2 = throwError $ "types do not unify: " ++ show t1 ++ " " ++ show t2

type Alt a = ([Pattern], Expr a)

typeDictionary :: [Decl SourceSpan] -> UnifyT (Type Name) Check ([Decl SourceSpan], [Decl SourceSpan], [Decl SourceSpan], [(Name, Type Name)])
typeDictionary vals = do
  let (untyped, typed) = (vals, [])
      untyped' = untyped
  untypedNames <- replicateM (length untyped) fresh
  let untypedDict = zip (map valueName untyped') untypedNames
  return (untyped', typed, [], untypedDict)
  where
  valueName (_ :< Value n _) = n

typeForBindingGroupEl :: Decl SourceSpan -> [(Name, Type Name)] -> UnifyT (Type Name) Check (Decl TypedAnn)
typeForBindingGroupEl (a :< Value name els) dict = do
  vals' <- forM els $ \(pats, val) -> do
    -- patTys <- inferPats pats
    val' <- bindNames dict (infer val)

    typeOf val' =?= fromJust (lookup name dict)
    return (pats, val')
  return $ Ann a (Type . typeOf . snd $ last vals') :< Value name vals'

replaceTypeVars :: [(Name, Type Name)] -> Type Name -> Type Name
replaceTypeVars subs (TVar n) = fromMaybe (TVar n) (n `lookup` subs)
replaceTypeVars subs (Arrow l r) = Arrow (replaceTypeVars subs l) (replaceTypeVars subs r)
replaceTypeVars subs (TAp f a) = TAp (replaceTypeVars subs f) (replaceTypeVars subs a)
replaceTypeVars subs a = a
