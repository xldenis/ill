module Ill.Inference.Substitution where

type Substitution = [(Tyvar, Type)]

class Types a where
  apply :: Substitution -> a -> a
  free  :: a -> [Tyvar]

instance Types a => Types [a] where
  apply s ts = map (apply s) ts
  free = nub . concat . map free

infixr 4 @@
(@@) = compose

compose :: Substitution -> Substitution -> Substitution
compose s1 s2 = over (traverse._2) (apply s1) s2 ++ s1

-- Make MonadFail when GHC 8 drops
merge :: Monad m => Substitution -> Substitution -> m Substitution
merge s1 s2 = if agree then return (s1 ++ s2) else fail "substitution merge failure"
  where agree = all (\x -> apply s1 (TVar x) == apply s2 (TVar x)) (map fst s1 `intersect` map fst s2)


