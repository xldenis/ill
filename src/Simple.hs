module Simple where
  import Control.Lens
  import Control.Lens.Setter
  import Data.List (union, intersect)
  import Control.Monad

  data Kind = Star | KFn Kind Kind
    deriving (Show, Eq)

  type Id = String

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
      (KFn k k') -> k'
    kind (TCon c) = kind c

  type Substitution = [(Tyvar, Type)]

  class Types a where
    apply :: Substitution -> a -> a
    free  :: a -> [Tyvar]

  instance Types Type where
    apply sbst free@(TVar t) = case lookup t sbst of
      Just tp -> tp
      Nothing -> free
    apply sbst (TAp l r) = TAp (apply sbst l) (apply sbst r)

    free (TVar t)  = [t]
    free (TAp l r) =  free l `union` free r
    free t         = []

  compose :: Substitution -> Substitution -> Substitution
  compose s1 s2 = over (traverse._2) (apply s1) s2 ++ s1


  -- Make MonadFail when GHC 8 drops
  merge :: Monad m => Substitution -> Substitution -> m Substitution
  merge s1 s2 = if agree then return (s1 ++ s2) else fail "substitution merge failure"
    where agree = all (\x -> apply s1 x == apply s2 x) (map fst s1 `intersect` map fst s2)


