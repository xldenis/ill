{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, PatternSynonyms, DeriveFunctor, DeriveFoldable, DeriveDataTypeable #-}
module Thrill.Syntax.Type where

import Thrill.Prelude

import Thrill.Syntax.Pretty
import Control.Monad.Unify (Unknown)
import Thrill.Syntax.Name
import Data.Data
import Data.String

data Type t
  = TVar t
  | TAp (Type t) (Type t)
  | TConstructor t
  | Arrow (Type t) (Type t)
  | Constrained [Constraint t] (Type t)
  | TUnknown Unknown
  | Forall [t] (Type t)
  | ArrowConstructor
  deriving (Eq, Show, Functor, Foldable, Data)

type Constraint t = (t, Type t)

instance Pretty (Type QualifiedName) where
  pretty = pretty . fmap unQualify
instance Pretty (Type String) where
  pretty (TVar var) = pretty var
  pretty (TAp (TAp (ArrowConstructor) a) b) = parensIf (complex a) (pretty a) <+> (pretty "->") <+> pretty b
  pretty (TAp f a) = pretty f <+> parensIf (complex a) (pretty a)
  pretty (TConstructor cons) = pretty cons
  pretty (Arrow from to) = parensIf (complex from) (pretty from) <+> pretty "->" <+> (pretty to)
  pretty (Constrained trts tp) = alternative (map prettyCons trts) <+> pretty tp
    where alternative = encloseSep mempty (mempty <+> pretty '|') (pretty ", ")
          prettyCons (nm, ty) = pretty nm <+> (pretty ty)
  pretty (TUnknown u) = pretty "unknown" <+> pretty (show u)
  pretty (Forall vars ty) = pretty "forall" <+> hsep (map pretty vars) <+> pretty "." <+> pretty ty

tArrow :: IsString a => Type a
tArrow =  ArrowConstructor

infixr 9 `tFn`

tFn :: Type a -> Type a -> Type a
tFn = Arrow

tString , tBool, tInteger, tDouble, tNil :: Type QualifiedName
tString   = TConstructor $ Qualified "Prelude" "String"
tBool     = TConstructor $ Qualified "Prelude" "Bool"
tInteger  = TConstructor $ Qualified "Prelude" "Int"
tDouble   = TConstructor $ Qualified "Prelude" "Double"
tNil      = TConstructor $ Qualified "Prelude" "Nil"
tChar     = TConstructor $ Qualified "Prelude" "Char"

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
    consVar (_, var) = freeVariables' var
  freeVariables' (TUnknown u) = []
  freeVariables' (Forall tyvars ty) = freeVariables ty \\ tyvars

varIfUnknown :: Type QualifiedName -> Type QualifiedName
varIfUnknown (TAp l r) = TAp (varIfUnknown l) (varIfUnknown r)
varIfUnknown (Arrow l r) = Arrow (varIfUnknown l) (varIfUnknown r)
varIfUnknown (Constrained ts t') = Constrained (map (fmap varIfUnknown) ts) (varIfUnknown t')
varIfUnknown (TUnknown u) = TVar toName
  where toName = Internal $ "a" ++ show u
varIfUnknown (Forall vars ty) = Forall vars (varIfUnknown ty)
varIfUnknown a = a

varsInType :: Eq a => Type a -> [a]
varsInType = nub . varsInType'
  where
  varsInType' (TAp l r) = varsInType' l ++ varsInType' r
  varsInType' (Arrow l r) = varsInType' l ++ varsInType' r
  varsInType' (Constrained ts t') = concatMap varsInType'' ts ++ varsInType' t'
    where varsInType'' (n, ty) = varsInType' ty
  varsInType' (TVar t) = [t]
  varsInType' (Forall vars t) = vars ++ varsInType' t
  varsInType' a = []

unconstrained :: Type a -> ([Constraint a], Type a)
unconstrained (Constrained cons t) = (concatMap unconstrainedConstraints cons, t)
  where
  unconstrainedConstraints (n, c) = let
    (cs', ty) = unconstrained c
    in (n, ty) : cs'
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

unwrapFnType :: Eq a => Type a -> [Type a]
unwrapFnType (Forall _ ty) = unwrapFnType ty
unwrapFnType t = unfoldr' go t
  where
  go (TAp (TAp ArrowConstructor a) b)  = (a, Just b)
  go (Arrow a b) = (a, Just b)
  go a           = (a, Nothing)

unfoldr' f b = case f b of
  (a, Just b') -> a : unfoldr' f b'
  (a, Nothing) -> [a]

unwrapN :: Int -> Type a -> [Type a]
unwrapN n (Forall _ ty) = unwrapN n ty
unwrapN n t = unfoldr' n go t
  where
  go (TAp (TAp (tArrow) a) b) = (a, Just b)
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
  where cs' = map (fmap (replaceTypeVars subs)) cs
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

  This function calculates the result of applying a type to a function type.
-}

applyArgumentToType :: Ord a => Type a -> Type a -> Maybe (Type a)
applyArgumentToType arg t@(Forall _ _) = Just $  applyTypeVars [arg] t
applyArgumentToType arg (TAp (TAp (ArrowConstructor) (TVar n)) b) = Just $ replaceTypeVars [(n, arg)] b
applyArgumentToType arg (Arrow (TVar n) b) = Just $ replaceTypeVars [(n, arg)] b
applyArgumentToType arg (TAp (TAp (ArrowConstructor) _) b) = Just b
applyArgumentToType arg (Arrow _ b) = Just b
applyArgumentToType arg t = Nothing
