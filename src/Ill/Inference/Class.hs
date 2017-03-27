module Ill.Inference.Class where
import Ill.Inference.Type

import Data.List (union)
import Data.Foldable

mguPred, matchPred :: Pred -> Pred -> Maybe Substitution
mguPred             = lift mgu
matchPred           = lift match

lift m (IsIn i t) (IsIn i' t')
         | i == i'   = m t t'
         | otherwise = fail "classes differ"

type Class    = ([Id], [Inst])
type Inst     = Qual Pred

data ClassEnv = ClassEnv { classes  :: Id -> Maybe Class,
                         defaults :: [Type] }

type Infer e t = ClassEnv -> [Assump] -> e -> TI ([Pred], t)

super     :: ClassEnv -> Id -> [Id]
super ce i = case classes ce i of Just (is, its) -> is

insts     :: ClassEnv -> Id -> [Inst]
insts ce i = case classes ce i of Just (is, its) -> its

defined :: Maybe a -> Bool
defined (Just x) = True
defined Nothing = False

modify :: ClassEnv -> Id -> Class -> ClassEnv
modify ce i c = ce {classes = \j -> if i==j then Just c else classes ce j}

initialEnv :: ClassEnv
initialEnv = ClassEnv { classes = \i -> fail "class not defined", defaults = []}

type EnvTransformer = ClassEnv -> Maybe ClassEnv

infixr 5 <:>
(<:>) :: EnvTransformer -> EnvTransformer -> EnvTransformer
(f <:> g) ce = do
  ce' <- f ce
  g ce'

addClass :: Id -> [Id] -> EnvTransformer
addClass i is ce
  | defined (classes ce i) = fail "class already defined"
  | any (not . defined . classes ce) is = fail "superclass not defined"
  | otherwise = return (modify ce i (is, []))

addPreludeClasses :: EnvTransformer
addPreludeClasses = Just


addInst :: [Pred] -> Pred -> EnvTransformer
addInst ps p@(IsIn i _) ce
 | not (defined (classes ce i)) = fail "no class for instance"
 | any (overlap p) qs           = fail "overlapping instance"
 | otherwise                    = return (modify ce i c)
   where its = insts ce i
         qs  = [ q | (_ :=> q) <- its ]
         c   = (super ce i, (ps:=>p) : its)

overlap :: Pred -> Pred -> Bool
overlap p q = defined (mguPred p q)


bySuper :: ClassEnv -> Pred -> [Pred]
bySuper ce p@(IsIn i t)
  = p : concat [ bySuper ce (IsIn i' t) | i' <- super ce i ]

byInst :: ClassEnv -> Pred -> Maybe [Pred]
byInst ce p@(IsIn i t) = msum [ tryInst it | it <- insts ce i ]
  where tryInst (ps :=> h) = do
                              u <- matchPred h p
                              return (map (apply u) ps)

entail        :: ClassEnv -> [Pred] -> Pred -> Bool
entail ce ps p = any (p `elem`) (map (bySuper ce) ps) ||
               case byInst ce p of
                 Nothing -> False
                 Just qs -> all (entail ce ps) qs

inHnf       :: Pred -> Bool
inHnf (IsIn c t) = hnf t
 where hnf (TVar v)  = True
       hnf (TCon tc) = False
       hnf (TAp t _) = hnf t

toHnfs      :: Monad m => ClassEnv -> [Pred] -> m [Pred]
toHnfs ce ps = do pss <- mapM (toHnf ce) ps
                  return (concat pss)

toHnf                 :: Monad m => ClassEnv -> Pred -> m [Pred]
toHnf ce p | inHnf p   = return [p]
           | otherwise = case byInst ce p of
                           Nothing -> fail "context reduction"
                           Just ps -> toHnfs ce ps

simplify   :: ClassEnv -> [Pred] -> [Pred]
simplify ce = loop []
 where loop rs []                            = rs
       loop rs (p:ps) | entail ce (rs++ps) p = loop rs ps
                      | otherwise            = loop (p:rs) ps

reduce      :: Monad m => ClassEnv -> [Pred] -> m [Pred]
reduce ce ps = do qs <- toHnfs ce ps
                  return (simplify ce qs)

scEntail        :: ClassEnv -> [Pred] -> Pred -> Bool
scEntail ce ps p = any (p `elem`) (map (bySuper ce) ps)
