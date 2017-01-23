module Ill.Inference.Type where
import Control.Lens

import qualified Ill.Syntax as Syntax (Name)

import Data.List (union, nub, intersect)
import Data.Maybe (fromMaybe)

type Id = Syntax.Name

data Kind
  = Star
  | KFun Kind Kind
  deriving (Show, Eq)

data Type = TVar Tyvar | TAp Type Type | TCon Tycon | TGen Int deriving (Show, Eq)

data Tyvar = Tyvar Id Kind deriving (Show, Eq)

data Tycon = Tycon Id Kind deriving (Show, Eq)

class HasKind k where
  kind :: k -> Kind

instance HasKind Tyvar where
  kind (Tyvar _ k) = k

instance HasKind Tycon where
  kind (Tycon _ k) = k

instance HasKind Type where
  kind (TVar t) = kind t
  kind (TAp l r) = case kind l of -- l :: k -> k', r :: k => l (r) :: k'
    (KFun k k') -> k'
  kind (TCon c) = kind c

type Substitution = [(Tyvar, Type)]

nullSubst  ::  Substitution
nullSubst   = []

class Types a where
  apply :: Substitution -> a -> a
  free  :: a -> [Tyvar]

instance Types a => Types [a] where
  apply s ts = map (apply s) ts
  free = nub . concatMap free

infixr 4 @@
(@@) = compose

compose :: Substitution -> Substitution -> Substitution
compose s1 s2 = over (traverse._2) (apply s1) s2 ++ s1

-- Make MonadFail when GHC 8 drops
merge :: Monad m => Substitution -> Substitution -> m Substitution
merge s1 s2 = if agree then return (s1 ++ s2) else fail "substitution merge failure"
  where agree = all (\x -> apply s1 (TVar x) == apply s2 (TVar x)) (map fst s1 `intersect` map fst s2)

instance Types Type where
  apply subst fv@(TVar tyvar) = fromMaybe fv (lookup tyvar subst)
  apply subst (TAp t1 t2) = TAp (apply subst t1) (apply subst t2)
  apply _ a = a

  free (TVar a) = [a]
  free (TAp t1 t2) = free t1 `union` free t2
  free _ = []

mgu :: Monad m => Type -> Type -> m Substitution
mgu (TAp l r) (TAp l' r') = do
  s1 <- mgu l l'
  s2 <- mgu (apply s1 r) (apply s1 r')
  s1 `merge` s2
mgu (TVar v) t = bindVar v t
mgu t (TVar v) = bindVar v t
mgu (TCon c1) (TCon c2) | c1 == c2 = return []
mgu _ _ = fail ""

bindVar :: Monad m => Tyvar -> Type -> m Substitution
bindVar v t | t == TVar v       = return []
            | v `elem` free t   = fail "a"
            | kind v /= kind t  = fail "aaa"
            | otherwise         = return [(v, t)]


match :: Monad m => Type -> Type -> m Substitution
match (TAp l r) (TAp l' r') = do
  sl <- match l l'
  sr <- match r r'
  sl `merge` sr
match (TVar v) t| kind v == kind t = return [(v,t)]
match (TCon c1) (TCon c2) | c1 == c2 = return []
match _ _ = fail ""




