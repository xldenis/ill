module Ill.Inference.Type where
import Control.Lens
import Control.Monad

-- import qualified Ill.Syntax as Syntax (Name)

import Data.List (union, nub, intersect)
import Data.Maybe (fromMaybe)

type Id = String

data Kind
  = Star
  | KFun Kind Kind
  deriving (Show, Eq)

data Type = TVar Tyvar | TAp Type Type | TCon Tycon | TGen Int deriving (Show, Eq)

data Tyvar = Tyvar Id Kind deriving (Show, Eq)

data Tycon = Tycon Id Kind deriving (Show, Eq)

data Pred = IsIn Id Type deriving (Show, Eq)

data Qual t = [Pred] :=> t deriving (Show, Eq)


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
mgu a b = fail $ "can't find most general unifer: " ++ (show a) ++ "///" ++ (show b)

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
match _ _ = fail "cant find a matching substitution"

data Scheme = Forall [Kind] (Qual Type)
            deriving (Show, Eq)

instance Types Scheme where
  apply s (Forall ks qt) = Forall ks (apply s qt)
  free (Forall ks qt)      = free qt

quantify      :: [Tyvar] -> Qual Type -> Scheme
quantify vs qt = Forall ks (apply s qt)
  where vs' = [ v | v <- free qt, v `elem` vs ]
        ks  = map kind vs'
        s   = zip vs' (map TGen [0..])

toScheme      :: Type -> Scheme
toScheme t     = Forall [] ([] :=> t)

data Assump = Id :>: Scheme deriving (Show, Eq)

instance Types Assump where
  apply s (i :>: sc) = i :>: apply s sc
  free (i :>: sc)      = free sc

find                 :: Monad m => Id -> [Assump] -> m Scheme
find i []             = fail ("unbound identifier: " ++ i)
find i ((i':>:sc):as) = if i==i' then return sc else find i as


newtype TI a = TI (Substitution -> Int -> (Substitution, Int, a))

instance Functor TI where
  fmap = liftM

instance Applicative TI where
  pure x = TI (\s n -> (s,n,x))
  (<*>) = ap

instance Monad TI where
  return   = pure
  TI f >>= g = TI (\s n -> case f s n of
                            (s',m,x) -> let TI gx = g x
                                        in  gx s' m)

runTI       :: TI a -> a
runTI (TI f) = x where (s,n,x) = f nullSubst 0

getSubst   :: TI Substitution
getSubst    = TI (\s n -> (s,n,s))

unify      :: Type -> Type -> TI ()
unify t1 t2 = do s <- getSubst
                 u <- mgu (apply s t1) (apply s t2)
                 extSubst u

extSubst   :: Substitution -> TI ()
extSubst s' = TI (\s n -> (s'@@s, n, ()))

enumId  :: Int -> Id
enumId n = "v" ++ show n

newTVar    :: Kind -> TI Type
newTVar k   = TI (\s n -> let v = Tyvar (enumId n) k
                          in  (s, n+1, TVar v))

freshInst               :: Scheme -> TI (Qual Type)
freshInst (Forall ks qt) = do ts <- mapM newTVar ks
                              return (inst ts qt)

class Instantiate t where
  inst  :: [Type] -> t -> t
instance Instantiate Type where
  inst ts (TAp l r) = TAp (inst ts l) (inst ts r)
  inst ts (TGen n)  = ts !! n
  inst ts t         = t
instance Instantiate a => Instantiate [a] where
  inst ts = map (inst ts)
instance Instantiate t => Instantiate (Qual t) where
  inst ts (ps :=> t) = inst ts ps :=> inst ts t
instance Instantiate Pred where
  inst ts (IsIn c t) = IsIn c (inst ts t)

instance Types Pred where
  apply sbst (IsIn n t) = IsIn n $ apply sbst t
  free (IsIn n t) = free t

instance Types t => Types (Qual t) where
  apply sbst (p :=> t) = apply sbst p :=> apply sbst t
  free (p :=> t) = free p `union` free t


fn :: Type -> Type -> Type
fn a b = TAp (TAp tArrow a) b

list       :: Type -> Type
list       = TAp tList

var :: Id -> Type
var id = TVar (Tyvar id Star)

tUnit    = TCon (Tycon "()" Star)

tList    = TCon (Tycon "[]" (KFun Star Star))
tArrow   = TCon (Tycon "(->)" (KFun Star (KFun Star Star)))
tTuple2  = TCon (Tycon "(,)" (KFun Star (KFun Star Star)))
