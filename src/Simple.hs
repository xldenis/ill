module Simple where
  import Control.Lens
  import Control.Lens.Setter
  import Data.List (union, intersect, (\\), partition, nub)
  import Control.Monad

  -- Basically all of this code is exploratory and taken from https://gist.github.com/chrisdone/0075a16b32bfd4f62b7b

  data Kind = Star | KFn Kind Kind
    deriving (Show, Eq)

  type Id = String

  enumId  :: Int -> Id
  enumId n = "v" ++ show n

  data Type = TVar Tyvar | TAp Type Type | TCon Tycon | TGen Int deriving (Show, Eq)

  data Tyvar = Tyvar Id Kind deriving (Show, Eq)

  data Tycon = Tycon Id Kind deriving (Show, Eq)

  fn :: Type -> Type -> Type
  fn = TAp . TAp (TCon $ Tycon "->" (KFn Star (KFn Star Star)))

  list       :: Type -> Type
  list t      = TAp tList t

  var :: Id -> Type
  var id = TVar (Tyvar id Star)

  tUnit    = TCon (Tycon "()" Star)
  tChar    = TCon (Tycon "Char" Star)
  tInt     = TCon (Tycon "Int" Star)
  tInteger = TCon (Tycon "Integer" Star)
  tFloat   = TCon (Tycon "Float" Star)
  tDouble  = TCon (Tycon "Double" Star)

  tList    = TCon (Tycon "[]" (KFn Star Star))
  tArrow   = TCon (Tycon "(->)" (KFn Star (KFn Star Star)))
  tTuple2  = TCon (Tycon "(,)" (KFn Star (KFn Star Star)))

  tString    :: Type
  tString     = list tChar

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

  nullSubst  :: Substitution
  nullSubst   = []

  class Types a where
    apply :: Substitution -> a -> a
    fv  :: a -> [Tyvar]

  instance Types Type where
    apply sbst fv@(TVar t) = case lookup t sbst of
      Just tp -> tp
      Nothing -> fv
    apply sbst (TAp l r) = TAp (apply sbst l) (apply sbst r)
    apply _ a = a

    fv (TVar t)  = [t]
    fv (TAp l r) =  fv l `union` fv r
    fv t         = []

  instance Types a => Types [a] where
    apply s = map (apply s)
    fv      = nub . concat . map fv

  infixr 4 @@
  (@@) = compose
  compose :: Substitution -> Substitution -> Substitution
  compose s1 s2 = over (traverse._2) (apply s1) s2 ++ s1

  -- Make MonadFail when GHC 8 drops
  merge :: Monad m => Substitution -> Substitution -> m Substitution
  merge s1 s2 = if agree then return (s1 ++ s2) else fail "substitution merge failure"
    where agree = all (\x -> apply s1 (TVar x) == apply s2 (TVar x)) (map fst s1 `intersect` map fst s2)

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
              | v `elem` fv t   = fail "a"
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

-- CLASS STUFF STARTS HERE

  data Qual t = [Pred] :=> t deriving (Eq)

  data Pred = IsIn Id Type deriving Eq

  instance Types Pred where
    apply sbst (IsIn n t) = IsIn n $ apply sbst t
    fv (IsIn n t) = fv t

  instance Types t => Types (Qual t) where
    apply sbst (p :=> t) = apply sbst p :=> apply sbst t
    fv (p :=> t) = fv p `union` fv t

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
  addPreludeClasses = const Just []


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

-- SCHEMES

  data Scheme = Forall [Kind] (Qual Type)
              deriving Eq

  instance Types Scheme where
    apply s (Forall ks qt) = Forall ks (apply s qt)
    fv (Forall ks qt)      = fv qt

  quantify      :: [Tyvar] -> Qual Type -> Scheme
  quantify vs qt = Forall ks (apply s qt)
    where vs' = [ v | v <- fv qt, v `elem` vs ]
          ks  = map kind vs'
          s   = zip vs' (map TGen [0..])

  toScheme      :: Type -> Scheme
  toScheme t     = Forall [] ([] :=> t)

  data Assump = Id :>: Scheme

  instance Types Assump where
    apply s (i :>: sc) = i :>: (apply s sc)
    fv (i :>: sc)      = fv sc

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

  type Infer e t = ClassEnv -> [Assump] -> e -> TI ([Pred], t)

  data Literal = LitInt  Integer
               | LitChar Char
               | LitRat  Rational
               | LitStr  String

  tiLit            :: Literal -> TI ([Pred],Type)
  tiLit (LitChar _) = return ([], tChar)
  tiLit (LitInt _)  = do v <- newTVar Star
                         return ([IsIn "Num" v], v)
  tiLit (LitStr _)  = return ([], tString)
  tiLit (LitRat _)  = do v <- newTVar Star
                         return ([IsIn "Fractional" v], v)

  data Pat        = PVar Id
                  | PWildcard
                  | PAs  Id Pat
                  | PLit Literal
                  | PNpk Id Integer
                  | PCon Assump [Pat]

  tiPat :: Pat -> TI ([Pred], [Assump], Type)

  tiPat (PVar i) = do v <- newTVar Star
                      return ([], [i :>: toScheme v], v)
  tiPat PWildcard   = do v <- newTVar Star
                         return ([], [], v)
  tiPat (PAs i pat) = do (ps, as, t) <- tiPat pat
                         return (ps, (i:>:toScheme t):as, t)
  tiPat (PLit l) = do (ps, t) <- tiLit l
                      return (ps, [], t)
  tiPat (PNpk i k)  = do t <- newTVar Star
                         return ([IsIn "Integral" t], [i:>:toScheme t], t)
  tiPat (PCon (i:>:sc) pats) = do (ps,as,ts) <- tiPats pats
                                  t'         <- newTVar Star
                                  (qs :=> t) <- freshInst sc
                                  unify t (foldr fn t' ts)
                                  return (ps++qs, as, t')


  tiPats     :: [Pat] -> TI ([Pred], [Assump], [Type])
  tiPats pats = do psasts <- mapM tiPat pats
                   let ps = concat [ ps' | (ps',_,_) <- psasts ]
                       as = concat [ as' | (_,as',_) <- psasts ]
                       ts = [ t | (_,_,t) <- psasts ]
                   return (ps, as, ts)

  data Expr = Var   Id
            | Lit   Literal
            | Const Assump
            | Ap    Expr Expr
            | Let   BindGroup Expr

  tiExpr                       :: Infer Expr Type
  tiExpr ce as (Var i)          = do sc         <- find i as
                                     (ps :=> t) <- freshInst sc
                                     return (ps, t)
  tiExpr ce as (Simple.Const (i:>:sc)) = do (ps :=> t) <- freshInst sc
                                            return (ps, t)
  tiExpr ce as (Lit l)          = do (ps,t) <- tiLit l
                                     return (ps, t)
  tiExpr ce as (Ap e f)         = do (ps,te) <- tiExpr ce as e
                                     (qs,tf) <- tiExpr ce as f
                                     t       <- newTVar Star
                                     unify (tf `fn` t) te
                                     return (ps++qs, t)
  tiExpr ce as (Let bg e)       = do (ps, as') <- tiBindGroup ce as bg
                                     (qs, t)   <- tiExpr ce (as' ++ as) e
                                     return (ps ++ qs, t)

  type Alt = ([Pat], Expr)

  tiAlt                :: Infer Alt Type
  tiAlt ce as (pats, e) = do (ps, as', ts) <- tiPats pats
                             (qs,t)  <- tiExpr ce (as'++as) e
                             return (ps++qs, foldr fn t ts)

  tiAlts             :: ClassEnv -> [Assump] -> [Alt] -> Type -> TI [Pred]
  tiAlts ce as alts t = do psts <- mapM (tiAlt ce as) alts
                           mapM (unify t) (map snd psts)
                           return (concat (map fst psts))

  split :: Monad m => ClassEnv -> [Tyvar] -> [Tyvar] -> [Pred]
                        -> m ([Pred], [Pred])
  split ce fs gs ps = do ps' <- reduce ce ps
                         let (ds, rs) = partition (all (`elem` fs) . fv) ps'
                         rs' <- defaultedPreds ce (fs++gs) rs
                         return (ds, rs \\ rs')

  type Ambiguity       = (Tyvar, [Pred])

  ambiguities         :: ClassEnv -> [Tyvar] -> [Pred] -> [Ambiguity]
  ambiguities ce vs ps = [ (v, filter (elem v . fv) ps) | v <- fv ps \\ vs ]

  numClasses :: [Id]
  numClasses  = ["Num", "Integral", "Floating", "Fractional", "Real", "RealFloat", "RealFrac"]

  stdClasses :: [Id]
  stdClasses  = ["Eq", "Ord", "Show", "Read", "Bounded", "Enum", "Ix",
                 "Functor", "Monad", "MonadPlus"] ++ numClasses

  candidates           :: ClassEnv -> Ambiguity -> [Type]
  candidates ce (v, qs) = [ t' | let is = [ i | IsIn i t <- qs ]
                                     ts = [ t | IsIn i t <- qs ],
                                 all ((TVar v)==) ts,
                                 any (`elem` numClasses) is,
                                 all (`elem` stdClasses) is,
                                 t' <- defaults ce,
                                 all (entail ce []) [ IsIn i t' | i <- is ] ]

  withDefaults :: Monad m => ([Ambiguity] -> [Type] -> a)
                    -> ClassEnv -> [Tyvar] -> [Pred] -> m a
  withDefaults f ce vs ps
      | any null tss  = fail "cannot resolve ambiguity"
      | otherwise     = return (f vps (map head tss))
        where vps = ambiguities ce vs ps
              tss = map (candidates ce) vps

  defaultedPreds :: Monad m => ClassEnv -> [Tyvar] -> [Pred] -> m [Pred]
  defaultedPreds  = withDefaults (\vps ts -> concat (map snd vps))

  defaultSubst   :: Monad m => ClassEnv -> [Tyvar] -> [Pred] -> m Substitution
  defaultSubst    = withDefaults (\vps ts -> zip (map fst vps) ts)

  type Expl = (Id, Scheme, [Alt])

  tiExpl :: ClassEnv -> [Assump] -> Expl -> TI [Pred]
  tiExpl ce as (i, sc, alts)
          = do (qs :=> t) <- freshInst sc
               ps         <- tiAlts ce as alts t
               s          <- getSubst
               let qs'     = apply s qs
                   t'      = apply s t
                   fs      = fv (apply s as)
                   gs      = fv t' \\ fs
                   sc'     = quantify gs (qs':=>t')
                   ps'     = filter (not . entail ce qs') (apply s ps)
               (ds,rs)    <- split ce fs gs ps'
               if sc /= sc' then
                   fail "signature too general"
                 else if not (null rs) then
                   fail "context too weak"
                 else
                   return ds

  type Impl   = (Id, [Alt])

  restricted   :: [Impl] -> Bool
  restricted bs = any simple bs
   where simple (i,alts) = any (null . fst) alts

  tiImpls         :: Infer [Impl] [Assump]
  tiImpls ce as bs = do ts <- mapM (\_ -> newTVar Star) bs
                        let is    = map fst bs
                            scs   = map toScheme ts
                            as'   = zipWith (:>:) is scs ++ as
                            altss = map snd bs
                        pss <- sequence (zipWith (tiAlts ce as') altss ts)
                        s   <- getSubst
                        let ps'     = apply s (concat pss)
                            ts'     = apply s ts
                            fs      = fv (apply s as)
                            vss     = map fv ts'
                            gs      = foldr1 union vss \\ fs
                        (ds,rs) <- split ce fs (foldr1 intersect vss) ps'
                        if restricted bs then
                            let gs'  = gs \\ fv rs
                                scs' = map (quantify gs' . ([]:=>)) ts'
                            in return (ds++rs, zipWith (:>:) is scs')
                          else
                            let scs' = map (quantify gs . (rs:=>)) ts'
                            in return (ds, zipWith (:>:) is scs')

  type BindGroup  = ([Expl], [[Impl]])

  tiBindGroup :: Infer BindGroup [Assump]
  tiBindGroup ce as (es,iss) =
    do let as' = [ v:>:sc | (v,sc,alts) <- es ]
       (ps, as'') <- tiSeq tiImpls ce (as'++as) iss
       qss        <- mapM (tiExpl ce (as''++as'++as)) es
       return (ps++concat qss, as''++as')

  tiSeq                  :: Infer bg [Assump] -> Infer [bg] [Assump]
  tiSeq ti ce as []       = return ([],[])
  tiSeq ti ce as (bs:bss) = do (ps,as')  <- ti ce as bs
                               (qs,as'') <- tiSeq ti ce (as'++as) bss
                               return (ps++qs, as''++as')

  type Program = [BindGroup]

  tiProgram :: ClassEnv -> [Assump] -> Program -> [Assump]
  tiProgram ce as bgs = runTI $
                        do (ps, as') <- tiSeq tiBindGroup ce as bgs
                           s         <- getSubst
                           rs        <- reduce ce (apply s ps)
                           s'        <- defaultSubst ce [] rs
                           return (apply (s'@@s) as')

