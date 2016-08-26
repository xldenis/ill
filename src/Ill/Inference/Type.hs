module Ill.Inference.Type where

  import qualified Ill.Syntax as Syntax (Name)
  import qualified Ill.Syntax as ST

  import Data.List (union, nub)

  type Id = Syntax.Name

  data Kind
    = Star
    | KFun Kind Kind
    deriving (Show, Eq)

  data Type = TVar Tyvar | TAp Type Type | TCon Tycon | TGen Int deriving (Show, Eq)

  data Tyvar = Tyvar Id Kind deriving (Show, Eq)

  data Tycon = Tycon Id Kind deriving (Show, Eq)

  typeFromSyntax :: ST.Type Id -> Type
  typeFromSyntax (ST.TVar t) = (TVar (Tyvar t Star))
  typeFromSyntax (ST.Arrow a b) = fn (typeFromSyntax a) (typeFromSyntax b)
  typeFromSyntax (ST.Constructor n args) = TCon (Tycon n (kfn $ length args))
    where kfn 0 = Star
          kfn n = KFun Star (kfn $ n-1)

  fn :: Type -> Type -> Type
  fn = TAp . TAp (TCon $ Tycon "->" (KFun Star (KFun Star Star)))

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

  class Types a where
    apply :: Substitution -> a -> a
    free  :: a -> [Tyvar]

  instance Types a => Types [a] where
    apply s ts = map (apply s) ts
    free = nub . concat . map free


  instance Types Type where
    apply subst fv@(TVar tyvar) = case lookup tyvar subst of
      Just subst -> subst
      Nothing -> fv
    apply subst (TAp t1 t2) = TAp (apply subst t1) (apply subst t2)
    apply _ a = a

    free (TVar a) = [a]
    free (TAp t1 t2) = (free t1) `union` (free t2)
    free _ = []



