{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, PatternSynonyms, DeriveFunctor, DeriveFoldable #-}
module Ill.Syntax.Type where

import Ill.Syntax.Pretty
import Control.Monad.Unify (Unknown)
import Data.Maybe
import Data.List

data Type t
  = TVar t
  | TAp (Type t) (Type t)
  | TConstructor t
  | Arrow (Type t) (Type t)
  | Constrained [Constraint t] (Type t)
  | TUnknown Unknown
  | Forall [t] (Type t)
  deriving (Eq, Show, Functor, Foldable)

type Constraint t = (t, [Type t])

instance Pretty (Type String) where
  pretty (TVar var) = pretty var
  pretty (TAp (TAp (TConstructor "->") a) b) = parensIf (complex a) (pretty a) <+> (pretty "->") <+> (pretty b)
  pretty (TAp f a) = pretty f <+> parensIf (complex a) (pretty a)
  pretty (TConstructor cons) = pretty cons
  pretty (Arrow from to) = parensIf (complex from) (pretty from) <+> pretty "->" <+> (pretty to)
  pretty (Constrained trts tp) = alternative (map prettyCons trts) <+> pretty tp
    where alternative = encloseSep mempty (mempty <+> pretty '|') (pretty ", ")
          prettyCons (nm, ts) = pretty nm <+> hsep (map pretty ts)
  pretty (TUnknown u) = pretty "unknown" <+> pretty (show u)
  pretty (Forall vars ty) = pretty "forall" <+> hsep (map pretty vars) <+> pretty "." <+> pretty ty

tArrow :: Type String
tArrow = TConstructor "->"

tFn :: Type String -> Type String -> Type String
tFn = Arrow

tString :: Type String
tString = TConstructor "String"
tBool = TConstructor "Bool"
tInteger = TConstructor "Int"
tDouble = TConstructor "Double"
tNil = TConstructor "Nil"

complex :: Type t -> Bool
complex (Arrow _ _) = True
complex (TAp _ _) = True
complex _ = False

generalize :: Type String -> Type String
generalize f@(Forall _ _) = f
generalize ty | null fv = ty
              | otherwise = Forall (freeVariables ty) ty
  where
  fv = freeVariables ty

freeVariables :: Eq t => Type t -> [t]
freeVariables = nub . freeVariables'
  where
  freeVariables' (TVar t) = [t]
  freeVariables' (TAp l r) = freeVariables' l ++ freeVariables' r
  freeVariables' (TConstructor con) = []
  freeVariables' (Arrow a b) = freeVariables' a ++ freeVariables' b
  freeVariables' (Constrained cons ty) = consVars cons ++ freeVariables' ty
    where
    consVars = concat . map consVar
    consVar (_, vars) = concat $ map freeVariables' vars
  freeVariables' (TUnknown u) = []
  freeVariables' (Forall tyvars ty) = freeVariables' ty \\ tyvars

varIfUnknown :: Type String -> Type String
varIfUnknown (TAp l r) = TAp (varIfUnknown l) (varIfUnknown r)
varIfUnknown (Arrow l r) = Arrow (varIfUnknown l) (varIfUnknown r)
varIfUnknown (Constrained ts t') = Constrained (map varIfUnknown' ts) (varIfUnknown t')
  where varIfUnknown' (a, ts) = (a, map varIfUnknown ts)
varIfUnknown (TUnknown u) = TVar toName
  where toName = "a" ++ show u
varIfUnknown (Forall vars ty) = Forall vars (varIfUnknown ty)
varIfUnknown a = a

varsInType :: Type String -> [String]
varsInType = nub . varsInType'
  where
  varsInType' (TAp l r) = varsInType' l ++ varsInType' r
  varsInType' (Arrow l r) = varsInType' l ++ varsInType' r
  varsInType' (Constrained ts t') = concatMap varsInType'' ts ++ varsInType' t'
    where varsInType'' (n, ts') = concatMap varsInType' ts'
  varsInType' (TVar t) = [t]
  varsInType' (Forall vars t) = vars ++ varsInType' t
  varsInType' a = []

unconstrained :: Type String -> ([Constraint String], Type String)
unconstrained (Constrained cons t) = (cons, t)
unconstrained (Forall vars ty) = let
  (cons, ty') = unconstrained ty
  in (cons, Forall vars ty')
unconstrained t@(TAp l r) = let
  (cons1, l') = unconstrained l
  (cons2, r') = unconstrained r
  in (cons1 ++ cons2, TAp l' r')
unconstrained t@(Arrow l r) = let
  (cons1, l') = unconstrained l
  (cons2, r') = unconstrained r
  in (cons1 ++ cons2, Arrow l' r')
unconstrained t = ([], t)

constrain :: [Constraint String] -> Type String -> Type String
constrain [] t = t
constrain cs (Constrained cs' t) = Constrained (nub $ cs ++ cs') t
constrain cs (Forall vars t) = Forall vars (constrain cs t)
constrain cs a = Constrained cs a

constraints :: Type String -> [Constraint String]
constraints = fst . unconstrained

flattenConstraints :: Type String -> Type String
flattenConstraints = uncurry constrain . unconstrained

unwrapFnType :: Type String -> [Type String]
unwrapFnType (Forall _ ty) = unwrapFnType ty
unwrapFnType t = unfoldr' go t
  where
  go (TAp (TAp (TConstructor "->") a) b) = (a, Just b)
  go (Arrow a b) = (a, Just b)
  go a           = (a, Nothing)

unfoldr' f b = case f b of
  (a, Just b') -> a : unfoldr' f b'
  (a, Nothing) -> [a]

unwrapN :: Int -> Type String -> [Type String]
unwrapN n (Forall _ ty) = unwrapN n ty
unwrapN n t = unfoldr' n go t
  where
  go (TAp (TAp (TConstructor "->") a) b) = (a, Just b)
  go (Arrow a b) = (a, Just b)
  go a           = (a, Nothing)

  unfoldr' 0 f b = [b]
  unfoldr' n f b = case f b of
    (a, Just b') -> a : unfoldr' (n - 1) f b'
    (a, Nothing) -> [a]

replaceTypeVars :: [(String, Type String)] -> Type String -> Type String
replaceTypeVars subs (TVar n) = fromMaybe (TVar n) (n `lookup` subs)
replaceTypeVars subs (Arrow l r) = Arrow (replaceTypeVars subs l) (replaceTypeVars subs r)
replaceTypeVars subs (TAp f a) = TAp (replaceTypeVars subs f) (replaceTypeVars subs a)
replaceTypeVars subs (Constrained cs a) = Constrained cs' (replaceTypeVars subs a)
  where cs' = map (fmap $ map (replaceTypeVars subs)) cs
replaceTypeVars subs f@(Forall vars ty) | not (null intersection) = replaceTypeVars (filter (\(n, _) -> not $ n `elem` intersection) subs) f
                                        | otherwise = Forall vars (replaceTypeVars subs ty)
  where
  keys = map fst subs
  intersection = keys \\ vars
  -- usedVars = concatMap (usedTypeVariables . snd) m
replaceTypeVars subs a = a

unwrapProduct :: Type String -> [Type String]
unwrapProduct ty = reverse $ unfoldr' go ty
  where
  go (TAp f a) = (a, Just f)
  go a         = (a, Nothing)
