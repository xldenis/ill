module Ill.Infer
( module Ill.Infer
, runCheck
, defaultCheckEnv
) where

import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Unify

import           Data.Coerce
import           Data.List
import           Data.Map             as M (union)
import           Data.Maybe

import           Text.Megaparsec      (initialPos)

import           Ill.Desugar
import           Ill.Error

import           Ill.Infer.Entail
import           Ill.Infer.Kind
import           Ill.Infer.Monad
import           Ill.Infer.Types

import           Ill.Parser.Lexer     (SourceSpan (..))
import           Ill.Syntax
import           Ill.Syntax.Type


type RawDecl = Decl SourceSpan

{-
  1. kind checking not implemented
  2. error messages suuuuuck
  3. Ensure no DAG in trait decls!
-}

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

    values <- forM (appSubs v') $ \(ann :< v) -> do
      let t = varIfUnknown $ (\(Type a) -> a) (ty ann)

      t' <- flattenConstraints <$> simplify' t

      addValue (valueName v) t'
      return $ (ann { ty = Type t' }) :< v

    return $ ValueBG values

    where

    valueName (Value n _) = n

    appSubs (ts, sub) = map (nestedFmap (\a -> a { ty = fmapTy (($?) sub) (ty a) })) ts

    fmapTy f (Type t) = Type (f t)
    fmapTy f t        = t

    simplify' (Constrained cons t) = do
      cons' <- reduce cons
      return $ Constrained cons' t
    simplify' t = return t

  go d@(DataBG  ds)                = do
    let dataDecls = map (\(_ :< Data nm args cons) -> (nm, args, map consPair cons)) ds

    kinds <- kindsOfAll [] (map (\(nm, param, cons) -> (nm, param, concatMap snd cons)) dataDecls)

    forM_ (zip dataDecls kinds) $ \((name, args, ctors), ctorKind) ->
      addDataType name args ctors ctorKind

    ds' <- forM (zip ds kinds) $ \(span :< d, k) -> do
      d' <- addAnn d
      return $ Ann span (Kind k) :< d'
    return (DataBG ds')
    where
    addAnn :: Declaration SourceSpan (Decl SourceSpan) -> Check (Declaration TypedAnn (Decl TypedAnn))
    addAnn (Data n vars cons) = return $ Data n vars cons
    addAnn n             = throwError $ InternalError "Non data value found in data binding group"

    consPair :: Type Name -> (Name, [Type Name])
    consPair   = (\(TConstructor n, b) -> (n,b)) . fromJust . uncons . reverse . unfoldCons
    unfoldCons (TAp f a) = a : unfoldCons f
    unfoldCons a         = [a]

  go (OtherBG (_ :< TypeSynonym{})) = throwError $ NotImplementedError "oops"
  go (OtherBG (a :< Import q m n al)) = return $ OtherBG $ Ann a None :< Import q m n al
  go (OtherBG (a :< TraitDecl supers name args members)) = do
    let memTys = map toPair members
        members' = map annSigs members
    addTrait name supers args memTys

    return . OtherBG $ Ann a None :< TraitDecl supers name args members'
    where
    toPair (_ :< Signature nm ty) = (nm, ty)
    toPair _ = error "trait declaration contains non signature value"

    annSigs (a :< Signature nm ty) = Ann a (Type ty) :< Signature nm ty
    annSigs _ = error "trait declaration contains non signature value"

  go (OtherBG (a :< TraitImpl supers nm args ds)) = do
    (superTraits, varNms, memSigs) <- lookupTrait nm
    let subs = zip varNms args
        cons = map (\(nm, cs') -> (nm, map (replaceTypeVars subs) cs' )) superTraits
    unsatisfiedSupers <- reduce cons
    when (not $ null unsatisfiedSupers) . internalError $ "unsatisfied supertraits in instance: " ++ show unsatisfiedSupers

    let constraints' = (nm, args) : supers
        tySubs     = zip varNms args
        sigTys     = map (fmap (constrain constraints' . replaceTypeVars tySubs)) memSigs
        signatures = map (uncurry Signature) sigTys
        annotated  = map (\sig -> emptySpan :< sig) signatures

    (vals', _) <- liftUnify $ do
      (ut, et, dict, untypedDict) <- typeDictionary (annotated ++ ds)

      when (not (null ut) || not (null untypedDict)) $ internalError "there are implicitly typed members to trait impl"

      withTraitInstance nm supers args $
        forM et $ \e -> uncurry checkBindingGroupEl e dict

    addTraitInstance nm supers args

    return . OtherBG $ Ann a None :< TraitImpl supers nm args (filter isValue vals')

    where
    traitName (Constrained _ t) = traitName t
    traitName (TAp f _)         = traitName f
    traitName (TConstructor c)  = c

    emptySpan = SourceSpan (initialPos "") (initialPos "")

  go (OtherBG _)                 = throwError $ NotImplementedError "oops"

addValue :: Ident -> Type Name -> Check ()
addValue name ty = do
  env <- env <$> get
  let env' = env { names = (name, ty) : names env  }
  modify $ \s -> s { env = env' }

addDataType :: Name -> [Name] -> [(Name, [Type Name])] -> Kind -> Check ()
addDataType name args dctors ctorKind = do
  env <- env <$> get
  let value = ctorKind
  let env' = env { types = (name, ctorKind) : types env }
  forM_ dctors $ uncurry (addDataConstructor name args)

addDataConstructor :: Name -> [Name] -> Name -> [Type Name] -> Check ()
addDataConstructor name args dctor tys = do
  env <- env <$> get
  let retTy = foldl TAp (TConstructor name) (map TVar args)
      consTy = foldr tFn retTy tys
      fields = args
  putEnv $ env { constructors = (dctor, (name, consTy, fields)) : constructors env}
  return ()

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

  patTys <- replicateM numArgs fresh
  retTy <- fresh

  x <- forM els $ \(pats, val) -> do
    patDict <- inferPats (zip patTys pats)
    val' <- bindNames (patDict ++ dict) (infer val)

    cons <- typeOf val' `constrainedUnification` retTy
    return ((pats, val'), cons)

  let (vals', cons') = unzip x
  let fTy = foldr tFn (typeOf . snd $ last vals') patTys
      memberType = fromJust (lookup name dict)
  fTy `constrainedUnification` memberType

  return $ Ann a (Type (constrain (concat  cons') memberType)) :< Value name vals'

checkBindingGroupEl :: Type Name -> Decl SourceSpan -> TypedDict -> UnifyT (Type Name) Check (Decl TypedAnn)
checkBindingGroupEl ty (a :< Value name els) dict = do
  let (pats, _) = unzip els
      numArgs = length $ head pats
  when (any (/= numArgs) $ map length pats) . throwError $ InternalError "branches have different amounts of patterns"

  let (constraints, ty') = unconstrained ty
      unwrapped = unwrapN numArgs ty'
      argTys    = if length unwrapped > 1 then init unwrapped else []
      retTy     = last unwrapped

  x <- forM els $ \(pats, val) -> do
    let patTys = zip argTys pats

    patTys' <- inferPats patTys
    val' <- bindNames (patTys' ++ dict) (check retTy val)

    cons <- retTy `constrainedUnification` typeOf val'

    return (cons, (pats, val'))


  let (cons, vals') = unzip x

  minCons <- UnifyT . lift $ reduce (concat cons)

  blargh constraints minCons

  return $ Ann a (Type ty) :< Value name vals'

  where

  blargh :: [Constraint Name] -> [Constraint Name] -> UnifyT (Type Name) Check ()
  blargh given inferred = do
    sub <- unifyCurrentSubstitution <$> UnifyT get
    let subbed = map (subCons sub) inferred

    UnifyT . lift $ idk given subbed


  subCons sub (n, tys) = (n, map (sub $?) tys)
