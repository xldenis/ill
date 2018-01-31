{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, PatternSynonyms, DeriveFunctor, DeriveFoldable, DeriveDataTypeable #-}
module Ill.Syntax.Type where

import Ill.Prelude

import Ill.Syntax.Pretty
import Control.Monad.Unify (Unknown)
import Ill.Syntax.Name
import Data.Data

data Type t
  = TVar t
  | TAp (Type t) (Type t)
  | TConstructor t
  | Arrow (Type t) (Type t)
  | Constrained [Constraint t] (Type t)
  | TUnknown Unknown
  | Forall [t] (Type t)
  deriving (Eq, Show, Functor, Foldable, Data)

type Constraint t = (t, [Type t])

instance Pretty (Type String) where
  pretty (TVar var) = pretty var
  pretty (TAp (TAp (TConstructor "->") a) b) = parensIf (complex a) (pretty a) <+> (pretty "->") <+> pretty b
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

infixr 9 `tFn`

tFn :: Type a -> Type a -> Type a
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
complex (Forall _ _) = True
complex (Constrained _ _) = True
complex _ = False

generalize :: Ord a => Type a -> Type a
generalize f@(Forall _ _) = f
generalize ty | null fv = ty
              | otherwise = generalizeWith fv ty
  where
  fv = freeVariables ty

generalizeWith :: [a] -> Type a -> Type a
generalizeWith [] ty = ty
generalizeWith fv (Forall fvs ty) = Forall (fv ++ fvs) ty
generalizeWith fv ty = Forall fv ty

generalizeWithout :: Ord a => [a] -> Type a -> Type a
generalizeWithout vars (Forall boundVars t) = generalizeWith ((nub $ freeVariables t ++ boundVars) \\ vars) t
generalizeWithout vars ty = generalizeWith (freeVariables ty \\ vars) ty

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
  freeVariables' (Forall tyvars ty) = freeVariables ty \\ tyvars

varIfUnknown :: Type String -> Type String
varIfUnknown (TAp l r) = TAp (varIfUnknown l) (varIfUnknown r)
varIfUnknown (Arrow l r) = Arrow (varIfUnknown l) (varIfUnknown r)
varIfUnknown (Constrained ts t') = Constrained (map varIfUnknown' ts) (varIfUnknown t')
  where varIfUnknown' (a, ts) = (a, map varIfUnknown ts)
varIfUnknown (TUnknown u) = TVar toName
  where toName = "a" ++ show u
varIfUnknown (Forall vars ty) = Forall vars (varIfUnknown ty)
varIfUnknown a = a

varsInType :: Eq a => Type a -> [a]
varsInType = nub . varsInType'
  where
  varsInType' (TAp l r) = varsInType' l ++ varsInType' r
  varsInType' (Arrow l r) = varsInType' l ++ varsInType' r
  varsInType' (Constrained ts t') = concatMap varsInType'' ts ++ varsInType' t'
    where varsInType'' (n, ts') = concatMap varsInType' ts'
  varsInType' (TVar t) = [t]
  varsInType' (Forall vars t) = vars ++ varsInType' t
  varsInType' a = []

unconstrained :: Type a -> ([Constraint a], Type a)
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

constrain :: Eq a => [Constraint a] -> Type a -> Type a
constrain [] t = t
constrain cs (Constrained cs' t) = Constrained (nub $ cs ++ cs') t
constrain cs (Forall vars t) = Forall vars (constrain cs t)
constrain cs a = Constrained cs a

constraints :: Type a -> [Constraint a]
constraints = fst . unconstrained

flattenConstraints :: Eq a => Type a -> Type a
flattenConstraints = uncurry (constrain . nub) . unconstrained

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

{-
  This method applies types to the variables of a scheme. It relies on the implicit ordering of
  the variables in said scheme. Ideally type schemes should be kept in a normalized order.

-}
applyTypeVars :: Ord a => [Type a] -> Type a -> Type a
applyTypeVars tys t@(Forall vars ty) = generalizeWithout vars' (replaceTypeVars subst ty)
  where
  fvs = freeVariables t
  subst = zip vars tys
  vars' = (nub . concat $ map (\(v, t) -> freeVariables t) subst) ++ fvs
applyTypeVars subs ty = ty

replaceTypeVars :: Eq a => [(a, Type a)] -> Type a -> Type a
replaceTypeVars subs (TVar n) = fromMaybe (TVar n) (n `lookup` subs)
replaceTypeVars subs (Arrow l r) = Arrow (replaceTypeVars subs l) (replaceTypeVars subs r)
replaceTypeVars subs (TAp f a) = TAp (replaceTypeVars subs f) (replaceTypeVars subs a)
replaceTypeVars subs (Constrained cs a) = Constrained cs' (replaceTypeVars subs a)
  where cs' = map (fmap $ map (replaceTypeVars subs)) cs
replaceTypeVars subs f@(Forall vars ty)
  --  not (null intersection) = replaceTypeVars (filter (\(n, _) -> not $ n `elem` intersection) subs) f
  | otherwise = generalizeWith (vars \\ keys) (replaceTypeVars subs ty)
  where
  keys = map fst subs
  intersection = keys `intersect` vars
replaceTypeVars subs a = a

-- Verify that a type is strictly less general than a second one implements a rough form of <=
-- do something to handle foralls
subsume :: Eq a => Type a -> Type a -> Maybe [(a, Type a)]
subsume t1 t2 = pure nub <*> subsume' t1 t2
  where
  -- this isn't entirely correct?
  subsume' (Forall vs t) (Forall vs' t') = subsume' t t'
  subsume' (TAp l1 r1) (TAp l2 r2)       = (++) <$> subsume' l1 l2 <*> subsume' r1 r2
  subsume' (Arrow l1 r1) (Arrow l2 r2)   = (++) <$> subsume' l1 l2 <*> subsume' r1 r2
  subsume' (TVar v) t                    = pure [(v, t)]
  subsume' (TConstructor t1) (TConstructor t2) | t1 == t2 = pure []
  subsume' _ _ = Nothing

unwrapProduct :: Type a -> [Type a]
unwrapProduct ty = reverse $ unfoldr' go ty
  where
  go (TAp f a) = (a, Just f)
  go a         = (a, Nothing)

{-
  We often need to apply arguments to types whether those arguments are themselves other types
  (in the case of polymorphic types) or values (in the case of normal functions).

  This function calculates the result of applying a type to a function.
-}

applyArgumentToType :: Type Name -> Type Name -> Type Name
applyArgumentToType arg t@(Forall _ _) = applyTypeVars [arg] t
applyArgumentToType arg (TAp (TAp (TConstructor "->") (TVar n)) b) = replaceTypeVars [(n, arg)] b
applyArgumentToType arg (Arrow (TVar n) b) = replaceTypeVars [(n, arg)] b
applyArgumentToType arg (TAp (TAp (TConstructor "->") _) b) = b
applyArgumentToType arg (Arrow _ b) = b

