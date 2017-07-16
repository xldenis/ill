{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Ill.Infer where

import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Unify
import           Data.Coerce
import           Data.List (unfoldr, uncons, partition, elem, sortOn)
import           Data.Map             as M (union)
import           Data.Maybe

import           Ill.Error
import           Ill.Desugar
import           Ill.Infer.Kind
import           Ill.Infer.Monad
import           Ill.Parser.Lexer     (SourceSpan)

import           Ill.Syntax
import           Ill.Syntax.Type
import qualified Data.HashMap.Strict  as H

type RawDecl = Decl SourceSpan

typeCheck :: [BindingGroup SourceSpan] -> Check [BindingGroup TypedAnn]
typeCheck bgs = mapM go bgs
  where
  go :: BindingGroup SourceSpan -> Check (BindingGroup TypedAnn)
  go (ValueBG ds)                  = do
    v' <- liftUnify $ do
          (ut, et, dict, untypedDict) <- typeDictionary ds
          explicit <- forM et $ \e -> uncurry checkBindingGroupEl e dict
          implicit <- forM ut $ \e -> typeForBindingGroupEl e dict

          return $ explicit ++ implicit
    let t = appSubs v'

    t' <- forM t $ \v -> do
      let t :< v' = v
          t' = varIfUnknown $ (\(Type a) -> a) (ty t)
          tAnn' = t { ty = Type t' }
      addValue (valueName v) t'
      return $ tAnn' :< v'
    return $ ValueBG t'
    where valueName (_ :< Value n _) = n
          appSubs (ts, sub) = map (\t -> nestedFmap (\a -> a { ty = fmapTy (\t -> flattenConstraints $ sub $? t) (ty a) }) t) ts
          fmapTy f (Type t) = Type (f t)
          fmapTy f t = t

  go d@(DataBG  ds)                = do
    let dataDecls = map (\(_ :< Data nm args cons) -> (nm, args, map consPair cons)) ds

    kinds <- kindsOfAll [] (map (\(nm, param, cons) -> (nm, param, concatMap (snd) cons)) dataDecls)

    forM_ (zip dataDecls kinds) $ \((name, args, ctors), ctorKind) -> do
      addDataType name args ctors ctorKind

    ds' <- forM (zip ds kinds) $ \(span :< d, k) -> do
      d' <- addAnn d
      return $ (Ann span (Kind k)) :< d'
    return (DataBG ds')
    where
    addAnn :: Declaration (SourceSpan) (Decl SourceSpan) -> Check (Declaration TypedAnn (Decl TypedAnn))
    addAnn (Data n vars cons) = return $ Data n vars cons
    addAnn n             = throwError $ InternalError "Non data value found in data binding group"

    consPair :: Type Name -> (Name, [Type Name])
    consPair   = (\(TConstructor n, b) -> (n,b)) . fromJust . uncons . reverse . unfoldCons
    unfoldCons (TAp f a) = a : unfoldCons f
    unfoldCons a = [a]

  go (OtherBG (_ :< TypeSynonym _ _ _)) = throwError $ NotImplementedError "oops"
  go (OtherBG (a :< (Import q m n al))) = return $ OtherBG $ Ann a None :< (Import q m n al)
  go (OtherBG (a :< TraitDecl supers name args members)) = do
    let memTys = map toPair members
        members' = map annSigs members
    addTrait name supers args memTys

    return . OtherBG $ Ann a None :< (TraitDecl supers name args members')
    where
    toPair (_ :< Signature nm ty) = (nm, ty)
    toPair _ = error "trait declaration contains non signature value"

    annSigs (a :< Signature nm ty) = Ann a (Type ty) :< Signature nm ty
    annSigs _ = error "trait declaration contains non signature value"

  go (OtherBG (_ :< TraitImpl _ _))     = throwError $ NotImplementedError "oops"
  go (OtherBG (_))                 = throwError $ NotImplementedError "oops"

addValue :: Ident -> Type Name -> Check ()
addValue name ty = do
  env <- env <$> get
  let env' = env { names = (name, ty) : names env  }
  modify $ \s -> s { env = env' }

addDataType :: Name -> [Name] -> [(Name, [Type Name])] -> Kind -> Check ()
addDataType name args dctors ctorKind = do
  env <- env <$> get
  let value = (ctorKind)
  let env' = env { types = (name, ctorKind) : (types env) }
  forM_ dctors $ \(dctor, tys) ->
    addDataConstructor name args dctor tys

addDataConstructor :: Name -> [Name] -> Name -> [Type Name] -> Check ()
addDataConstructor name args dctor tys = do
  env <- env <$> get
  let retTy = foldl TAp (TConstructor name) (map TVar args)
      consTy = foldr tFn retTy tys
      fields = args
  putEnv $ env { constructors = (dctor, (name, consTy, fields)) : (constructors env)}
  return ()

data TypedAnn = Ann { span :: SourceSpan, ty :: TypeAnn }
  deriving (Show, Eq)

data TypeAnn
  = Type (Type Name)
  | Kind Kind
  | None
  deriving (Show, Eq)

infer :: Expr SourceSpan -> UnifyT (Type Name) Check (Expr TypedAnn)
infer (a :< Apply l args) = do
  f' <- infer l
  fTy <- freshenFunction (typeOf f')

  args' <- mapM infer args
  retTy <- fresh

  let (cons1, fTy')   = unconstrained fTy
      (cons2, argTy') = unconstrained $ foldr tFn retTy (map typeOf args')

  fTy' =?= argTy'

  let retTy' = flattenConstraints $ Constrained (cons1 ++ cons2) retTy

  return $ Ann a (Type retTy') :< Apply f' args'
infer (a :< If cond left right) = do
  cond' <- check tBool cond
  left' <- infer left
  right' <- infer right

  typeOf left' =?= typeOf right'

  return $ Ann a (Type $ typeOf left') :< If cond' left' right'
infer (a :< Case cond branches) = do
  cond' <- infer cond
  retTy <- fresh

  branches' <- forM branches $ \(pattern, expr) -> do
    dict <- inferPat (typeOf cond') pattern

    expr' <- bindNames dict (infer expr)
    typeOf expr' =?= retTy

    return $ (pattern, expr')

  return $ Ann a (Type . typeOf . snd $ last branches') :< Case cond' branches'
infer (a :< Body es) = do
  tys <- bindNames [] $ mapM infer es

  return $ Ann a (Type . typeOf $ last tys) :< Body tys
infer (a :< BinOp op l r) = do
  op' <- infer op
  l' <- infer l
  r' <- infer r
  tRet <- fresh

  typeOf op' =?= (typeOf l' `tFn` typeOf r' `tFn` tRet)

  return $ Ann a (Type tRet) :< BinOp op' l' r'
infer (a :< Lambda pats expr) = do
  patTys <- replicateM (length pats) fresh

  patDict <- inferPats (zip patTys pats)
  expr' <- bindNames patDict (infer expr)

  let retTy = foldr tFn (typeOf expr') patTys
  return $ Ann a (Type retTy) :< Lambda pats expr'
infer (a :< Assign lnames exps) = do
  exps' <- mapM infer exps
  let bound = zip lnames (map typeOf exps')
  modifyEnv $ \e -> e { names = bound ++ (names e) }

  return $ Ann a (Type tNil) :< Assign lnames exps'
infer (a :< Var nm) = do
  ty <- lookupVariable nm

  return $ Ann a (Type ty) :< Var nm
infer (a :< Constructor nm) = do
  (_, ty, args) <- lookupConstructor nm

  subs <- mapM (\a -> (,) <$> pure a <*> fresh) args

  let nT = replaceTypeVars subs ty

  return $ Ann a (Type nT) :< Constructor nm
infer (a :< Literal l) = do
  let ty = inferLit l
  return $ Ann a (Type ty) :< Literal l

typeOf :: Cofree a TypedAnn -> (Type Name)
typeOf (ann :< _) = fromType $ ty ann
  where fromType (Type t) = t

check :: Type Name -> Expr SourceSpan -> UnifyT (Type Name) Check (Expr TypedAnn)
check expected (a :< If cond l r) = do
  cond' <- check tBool cond
  left' <- check expected l
  right' <- check expected r

  return $ Ann a (Type expected) :< If cond' left' right'
check expected (a :< Var nm) = do
  ty <- lookupVariable nm
  ty =?= expected
  return $ Ann a (Type ty) :< Var nm
check expected (a :< Constructor nm) = do
  (_, ty, args) <- lookupConstructor nm

  subs <- mapM (\a -> (,) <$> pure a <*> fresh) args

  let nT = replaceTypeVars subs ty
  nT =?= expected

  return $ Ann a (Type nT) :< Constructor nm
check expected v@(_ :< BinOp _ _ _)= do
  rTy <- infer v
  (typeOf rTy) =?= expected
  return rTy
check expected (a :< Body es) = do
  tys <- mapM infer es

  let (cons, retTy)   = unconstrained (typeOf $ last tys)

  retTy =?= expected

  return $ Ann a (Type . typeOf $ last tys) :< Body tys

check expected ty = error (show ty)


instance Partial (Type a) where
  unknown                    = TUnknown

  isUnknown (TUnknown u) = Just u
  isUnknown _            = Nothing

  unknowns (TAp l r)         = unknowns l ++ unknowns r
  unknowns (Arrow l r)       = unknowns l ++ unknowns r
  unknowns (Constrained ts t) = concatMap unknowns' ts ++ unknowns t
    where unknowns' (nm, ts) = concatMap unknowns ts
  unknowns _                 = []

  ($?) sub (TAp l r)         = TAp (sub $? l) (sub $? r)
  ($?) sub (Arrow l r)       = Arrow (sub $? l) (sub $? r)
  ($?) sub (Constrained ts t) = Constrained (map sub' ts) (sub $? t)
    where sub' (nm, ts) = (nm, map (sub $?) ts)
  ($?) sub t@(TUnknown u)    = fromMaybe t $ H.lookup u (runSubstitution sub)
  ($?) sub other             = other

instance UnificationError (Type Name) MultiError where
  occursCheckFailed t = TypeOccursError t

instance Unifiable Check (Type Name) where
  (=?=) = unifyTypes

unifyTypes :: (Type Name) -> (Type Name) -> UnifyT (Type Name) Check ()
unifyTypes (TUnknown u1) (TUnknown u2) | u1 == u2 = return ()
unifyTypes (TUnknown u) t = u =:= t
unifyTypes t (TUnknown u) = u =:= t
unifyTypes (TVar v1) (TVar v2) | v1 == v2 = return ()
unifyTypes (TAp l1 r1) (TAp l2 r2) = do
  l1 `unifyTypes` l2
  r1 `unifyTypes` r2
unifyTypes (Arrow l1 r1) (Arrow l2 r2) = do
  l1 `unifyTypes` l2
  r1 `unifyTypes` r2
unifyTypes c1@(TConstructor cNm1) c2@(TConstructor cNm2) =
  if c1 == c2
  then return ()
  else throwError $ UnificationError c1 c2
unifyTypes t1 t2 = throwError $ UnificationError t1 t2

constrainedUnification :: Type Name -> Type Name -> UnifyT (Type Name) Check [Constraint Name]
constrainedUnification t1 t2 = let
  (cons1, t1') = unconstrained t1
  (cons2, t2') = unconstrained t2

  in t1' =?= t2' >> return (cons1 ++ cons2)

type Alt a = ([Pattern], Expr a)

inferLit (RawString _) = tString
inferLit (EscString _) = tString
inferLit (Integer _ )  = tInteger
inferLit (Double _)    = tDouble

type TypedDict   = [(Name, Type Name)]
type UntypedDict = [(Name, Type Name)]

typeDictionary :: [Decl SourceSpan] -> UnifyT (Type Name) Check ([Decl SourceSpan], [(Type Name, Decl SourceSpan)], TypedDict, UntypedDict)
typeDictionary vals = do
  let values = sortOn valueName $ filter isValue vals
      sigs =  sortOn signatureName $ filter isSignature vals
      sigNames = map signatureName sigs
      (typedVals, untyped) = partition (\v -> valueName v `elem` sigNames) values
      typed = zip (map signatureType sigs) typedVals

  untypedNames <- replicateM (length untyped) fresh
  let untypedDict = zip (map valueName untyped) untypedNames
      typedDict   = map (\(t, v) -> (valueName v, t)) typed 
  return (untyped, typed, typedDict ++ untypedDict, untypedDict)
  where
  valueName (_ :< Value n _) = n
  signatureName (_ :< Signature n _) = n
  signatureType (_ :< Signature _ t) = t

typeForBindingGroupEl :: Decl SourceSpan -> UntypedDict -> UnifyT (Type Name) Check (Decl TypedAnn)
typeForBindingGroupEl (a :< Value name els) dict = do
  let (pats, _) = unzip els
      numArgs = length $ head pats
  when (any (/= numArgs) $ map length pats) . throwError $ InternalError "branches have different amounts of patterns"

  patTys <- replicateM (numArgs) fresh
  retTy <- fresh

  vals' <- forM els $ \(pats, val) -> do
    patDict <- inferPats (zip patTys pats)
    val' <- bindNames (patDict ++ dict) (infer val)

    typeOf val' =?= retTy
    return (pats, val')

  let fTy = foldr tFn (typeOf . snd $ last vals') patTys
  fTy =?= fromJust (lookup name dict)

  return $ Ann a (Type $ fromJust (lookup name dict)) :< Value name vals'

checkBindingGroupEl ty (a :< Value name els) dict = do
  let (constraints, ty') = unconstrained ty
      unwrapped = unwrapFnType ty'
      argTys    = if length unwrapped > 1 then init unwrapped else []
      retTy     = last unwrapped

  vals' <- forM els $ \(pats, val) -> do
    let patTys = zip argTys pats

    patTys' <- inferPats patTys 
    val' <- bindNames (patTys' ++ dict) (check retTy val)

    cons <- retTy `constrainedUnification` typeOf val'
    return (pats, val')

  return $ Ann a (Type $ ty) :< Value name vals'


checkPats = undefined

inferPats pats = concat <$> mapM (uncurry inferPat) pats

inferPat :: Type Name -> Pattern -> UnifyT (Type Name) Check [(Name, Type Name)]
inferPat ty (PVar n) = do
  f <- fresh
  ty =?= f
  return [(n, f)]
inferPat ty (Destructor n pats) = do
  (_, t, _) <- lookupConstructor n
  freshened <- freshenFunction t
  go pats freshened
  where
  go :: [Pattern] -> Type Name -> UnifyT (Type Name) Check [(Name, Type Name)]
  go [] ty' = ty =?= ty' *> pure []
  go (pat : pats) (TAp (TAp f a) b) | f == tArrow =
    (++) <$> inferPat a pat <*> go pats b
  go (pat : pats) (Arrow a b) =
    (++) <$> inferPat a pat <*> go pats b
  go _ _ = throwError $ InternalError "Impossible"
inferPat ty (PLit lit) = do
  let litTy = inferLit lit
  litTy =?= ty
  return []

freshenFunction :: Type Name -> UnifyT (Type Name) Check (Type Name)
freshenFunction ty = do
  let vars = varsInType ty
  replacements <- forM vars $ \var -> (,) <$> pure var <*> fresh

  return $ foldr (\(i, varTy) t -> replaceTypeVars [(i,varTy)] t) ty replacements
