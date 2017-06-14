{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Ill.Infer where

import Ill.Infer.Monad
import Ill.Desugar
import Control.Monad.Except
import Control.Monad.Unify
import Control.Monad.State
import Ill.Syntax
import Ill.Parser.Lexer (SourceSpan)
import qualified Data.HashMap.Strict as H
import Data.Map as M (union)
import Data.Maybe

typeCheck :: [BindingGroup SourceSpan] -> Check [BindingGroup TypedAnn]
typeCheck bgs = mapM go bgs
  where
  go :: BindingGroup SourceSpan -> Check (BindingGroup TypedAnn)
  go (ValueBG ds)                  = do
    (t, _) <- liftUnify $ do
      (ut, _, dict, untypedDict) <- typeDictionary ds
      forM ut $ \e -> typeForBindingGroupEl e untypedDict
    return $ ValueBG t
  go (DataBG  ds)                  = throwError "oops"
  go (OtherBG (_ :< TypeSynonym _ _ _)) = throwError "oops"
  go (OtherBG (a :< (Import q m n al))) = return $ OtherBG $ Ann a tyBool :< (Import q m n al)
  go (OtherBG (_ :< TraitDecl _ _))     = throwError "oops"
  go (OtherBG (_ :< TraitImpl _ _))     = throwError "oops"
  go (OtherBG (_))                 = throwError "oops"

data TypedAnn = Ann { span :: SourceSpan, ty :: Type Name }
  deriving (Show, Eq)

infer :: Expr SourceSpan -> UnifyT (Type Name) Check (Expr TypedAnn)
infer (_ :< Apply l args) = throwError "oops"
infer (a :< If cond left right) = do
  cond' <- check cond tyBool
  left' <- infer left
  right' <- infer right
  t' <- mgu (typeOf left') (typeOf right')

  return $ Ann a t' :< If cond' left' right'
infer (a :< Body es) = do
  tys <- mapM infer es
  return $ Ann a (typeOf $ last tys) :< Body tys
infer (a :< Literal l) = do
  let ty = inferLit l
  return $ Ann a ty :< Literal l
  where
  inferLit (RawString _) = tString
  inferLit (EscString _) = tString
  inferLit (Integer _ )  = tInteger
  inferLit (Double _)    = tDouble

mgu :: Type Name -> Type Name -> UnifyT (Type Name) Check (Type Name)
mgu = undefined

typeOf :: Expr TypedAnn -> (Type Name)
typeOf (ann :< _) = ty ann

check :: Expr SourceSpan -> Type Name -> UnifyT (Type Name) Check (Expr TypedAnn)
check = undefined

tyBool = Constructor "Bool"
tInteger = Constructor "Int"
tDouble = Constructor "Double"
tString = Constructor "String"

instance Partial (Type a) where
  unknown = TUnknown
  isUnknown (TUnknown u) = Just u
  isUnknown _ = Nothing
  unknowns (TAp l r) = unknowns l ++ unknowns r
  unknowns (Arrow l r) = unknowns l ++ unknowns r
  unknowns (Trait _ r) = unknowns r
  unknowns (Constraint ts t) = concatMap unknowns ts ++ unknowns t
  unknowns _ = []
  ($?) sub (TAp l r) = TAp (sub $? l) (sub $? r)
  ($?) sub (Arrow l r) = Arrow (sub $? l) (sub $? r)
  ($?) sub (Trait n t) = Trait n (sub $? t)
  ($?) sub (Constraint ts t) = Constraint (map (sub $?) ts) (sub $? t)
  ($?) sub t@(TUnknown u) = fromMaybe t $ H.lookup u (runSubstitution sub)
  ($?) sub other = other

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
unifyTypes (Constructor c1) (Constructor c2) =
  if c1 == c2
  then return ()
  else throwError "types do not unify"
unifyTypes (Constraint ts1 t1) t2 = throwError "constrained type unification"
unifyTypes t1 (Constraint ts2 t2) = throwError "constrained type unification"

type Alt a = ([Pattern], Expr a)

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
  return $ Ann a (typeOf . snd $ last vals') :< Value name vals'

bindNames nms action = localState (\s -> s { env = (env s) { names = (names $ env s) ++ nms }}) action

