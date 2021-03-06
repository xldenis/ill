module Thrill.Infer
( module Thrill.Infer
, runCheck
, defaultCheckEnv
, Environment(..)
, ConstructorEntry(..)
, TraitEntry(..)
, CheckState(..)
, module Thrill.Error
) where

{-
  Entrypoint for type checker

  This module handles the portions of typechecking related to top level declarations.

  All declarations have been sorted into binding groups already.

  Typechecking is performed in an ordered manner:

  1. Gather all instances in module, bind them as if they are true
  2. Check data declarations
  3. Check trait declarations
  4. Check values, this does not include trait implementations
  5. Check trait implementations
  6. Ignore all other kinds of declarations

  When checking a value binding group, first the types of all values are bound before checking
  each value individually.
-}

import           Thrill.Prelude

import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Unify

import           Data.List
import           Data.Map             as M (union)

import           Text.Megaparsec      (initialPos)

import           Thrill.BindingGroup
import           Thrill.Renamer (renameModule)
import           Thrill.Error

import           Thrill.Infer.Entail
import           Thrill.Infer.Kind
import           Thrill.Infer.Monad
import           Thrill.Infer.Types

import           Thrill.Parser.Lexer     (SourceSpan (..))
import           Thrill.Syntax
import           Thrill.Syntax.Type
import           Data.Bitraversable
import           Control.Lens.Plated
import           Data.Bifunctor (bimap)

type RawDecl = Decl QualifiedName SourceSpan

{-
  1. kind checking not implemented
  2. error messages suck
-}

typeCheckModule :: ModuleBG QualifiedName SourceSpan -> Either (Error a) (Module QualifiedName TypedAnn, Environment)
typeCheckModule mod@(Module nm ds) = do
  typecheckedGroups <- execCheck $ typeCheck ds
  (typcheckedDecls, env) <-  pure $ bimap fromBindingGroups env typecheckedGroups
  return (Module nm typcheckedDecls, env)

typeCheck :: BoundModules QualifiedName SourceSpan -> Check [BindingGroup QualifiedName TypedAnn]
typeCheck (BoundModules
  { classDecls = classDecls
  , instDecls  = instDecls
  , valueDecls = valueDecls
  , otherDecls = otherDecls
  , dataDecls  = dataDecls
  }) = do
    mapM gatherInstInfo instDecls
    mapM go (dataDecls ++ classDecls ++ valueDecls ++ instDecls ++ otherDecls)

  where
  gatherInstInfo (OtherBG (_ :< TraitImpl supers nm args _)) = addTraitInstance nm supers args

  go :: BindingGroup QualifiedName SourceSpan -> Check (BindingGroup QualifiedName TypedAnn)
  go (ValueBG ds)                  = do
    inferredVals <- liftUnify $ do
      (ut, et, dict, untypedDict) <- typeDictionary (map generalizeSig ds)
      explicit <- forM et $ \e -> uncurry checkBindingGroupEl e dict
      implicit <- forM ut $ \e -> typeForBindingGroupEl e dict

      return $ explicit ++ implicit

    subbedValues <- appSubs inferredVals
    values <- forM subbedValues $ \(ann :< v) -> rethrow (ErrorInDecl (valueName v)) $ do
      let t = fromTyAnn ann

      t' <- flattenConstraints <$> simplify' t
      when (ambiguous t') . throwError $ AmbiguousType (map qualName $ ambiguities t') t'

      let generalizedType = generalize t'
      addValue (valueName v) generalizedType

      return $ (ann { ty = Type generalizedType Nothing }) :< updateValue (generalizeVars (valueName v) generalizedType) v

    return $ ValueBG values

    where

    updateValue f (Value nm brs) = Value nm (map (fmap $ transform f) brs)

    generalizeVars target t (a :< Var nm) | nm == target = a { ty = fmapTy (const t) (ty a) } :< Var nm
    generalizeVars _ _ a = a

    generalizeSig (a :< Signature nm ty) = a :< Signature nm (generalize ty)
    generalizeSig decl = decl

    valueName (Value n _) = n

    ambiguous ty = not . null $ ambiguities ty
    ambiguities ty = fvInConstraints \\ tyVars
      where
      tyVars = varsInType ty'
      (constraints, ty') = unconstrained ty
      fvInConstraints = nub $ concatMap (freeVariables . snd) constraints

    simplify' (Constrained cons t) = do
      cons' <- reduce cons
      return $ Constrained cons' t
    simplify' t = return t

  go d@(DataBG  ds)                = do
    let dataDecls = map fromDataDecl ds

    kinds <- kindsOfAll [] (map (\(nm, param, cons) -> (nm, param, concatMap snd cons)) dataDecls)

    forM_ (zip dataDecls kinds) $ \((name, args, ctors), ctorKind) ->
      addDataType name args ctors ctorKind

    ds' <- forM (zip ds kinds) $ \(span :< d, k) -> do
      return $ TyAnn (pure span) (Kind k) :< coerceAnn d
    return (DataBG ds')
    where

    fromDataDecl (_ :< Data nm args cons) = (nm, args, map consPair cons)
    fromDataDecl _ = error "impossible non-data value found in data binding group"

    coerceAnn :: Declaration QualifiedName SourceSpan (RawDecl) -> Declaration QualifiedName TypedAnn (Decl QualifiedName TypedAnn)
    coerceAnn (Data n vars cons) = Data n vars cons
    coerceAnn _ = error "impossible non-data value found in data binding group"

    consPair :: Type QualifiedName -> (QualifiedName, [Type QualifiedName])
    consPair   = fromCons . fromJust . uncons . unwrapProduct

    fromCons (TConstructor n, b) = (n,b)
    fromCons _ = error "non constructor value found"

  go (OtherBG (_ :< TypeSynonym{})) = throwError $ NotImplementedError "oops"
  go (OtherBG (a :< Import q m n al)) = return $ OtherBG $ TyAnn (pure a) None :< Import q m n al
  go (OtherBG (a :< TraitDecl supers name args members)) = rethrow (ErrorInDecl name) $ do
    let memTys = map toPair members
        members' = map annSigs members

    addTrait name supers args memTys

    return . OtherBG $ TyAnn (pure a) None :< TraitDecl supers name args members'
    where
    toPair (_ :< Signature nm ty) = (nm, ty)
    toPair _ = error "trait declaration contains non signature value"

    annSigs (a :< Signature nm ty) = Ann a ty :< Signature nm ty
    annSigs _ = error "trait declaration contains non signature value"

  go (OtherBG (a :< TraitImpl supers nm args ds)) = rethrow (ErrorInDecl nm) $ do
    trait <- lookupTrait nm
    let subs = [(traitVarNm trait, args)]
        cons = map (fmap (replaceTypeVars subs)) (superTraits trait)

    unsatisfiedSupers <- reduce cons
    when (not $ null unsatisfiedSupers) . throwError $ MissingSuperTraits unsatisfiedSupers nm args

    let missingTraitMethods = map fst (methodSigs trait) \\ map valueName ds

    when (not (null missingTraitMethods)) $ do
      let missingTraitMethodsDetailed = filter (\(name, _) -> name `elem` missingTraitMethods) (methodSigs trait)
      throwError $ MissingTraitImplMethods nm args missingTraitMethodsDetailed

    let constraints' = (nm, args) : supers
        argVars    = nub $ freeVariables args
        sigTys     = map (fmap (generalizeWithout argVars . constrain constraints' . applyTypeVars [args])) (methodSigs trait)
        signatures = map (uncurry Signature) sigTys
        annotated  = map (\sig -> emptySpan :< sig) signatures

    vals' <- liftUnify $ do
      (ut, et, dict, untypedDict) <- typeDictionary (annotated ++ ds)

      when (not (null ut) || not (null untypedDict)) . throwError $ UnknownTraitMethods nm args (map valueName ut)

      {-
        unncessary since all traits are bound already
      -}
      withTraitInstance nm supers args $
        forM et $ \e -> uncurry checkBindingGroupEl e []

    addTraitInstance nm supers args

    subbedValues <- appSubs vals'
    values <- forM (subbedValues) $ \(ann :< v) -> do
      let t = varIfUnknown $ fromType (ty ann)

      t' <- flattenConstraints <$> simplify' t
      return $ (ann { ty = Type t' Nothing }) :< v


    return . OtherBG $ TyAnn (pure a) None :< TraitImpl supers nm args (filter isValue values)

    where
    traitName (Constrained _ t) = traitName t
    traitName (TAp f _)         = traitName f
    traitName (TConstructor c)  = c

    simplify' (Constrained cons t) = do
      cons' <- reduce cons
      return $ Constrained cons' t
    simplify' (Forall vars ty) = Forall vars <$> simplify' ty
    simplify' t = return t

    emptySpan = SourceSpan (initialPos "") (initialPos "")

  go (OtherBG _)                 = throwError $ NotImplementedError "oops"

appSubs :: ([Decl QualifiedName TypedAnn], Substitution (Type QualifiedName)) -> Check [Decl QualifiedName TypedAnn]
appSubs (ts, sub) = mapM (substituteOneDecl sub) ts

substituteOneDecl :: Substitution (Type QualifiedName) -> Decl QualifiedName TypedAnn -> Check (Decl QualifiedName TypedAnn)
substituteOneDecl sub decl = hoistAppToCofree (substituteAnn sub) $ decl

substituteAnn :: Substitution (Type QualifiedName) -> TypedAnn -> Check (TypedAnn)
substituteAnn sub = \ann -> do
  let (Type poly inst) = ty ann
      subbedPoly = substituteOneType sub poly
      subbedInst = substituteOneType sub <$> inst

  pure $ ann { ty = Type subbedPoly subbedInst }

substituteOneType :: Substitution (Type QualifiedName) -> Type QualifiedName -> Type QualifiedName
substituteOneType sub ty = varIfUnknown $ sub $? ty

type TypedDict   = [(QualifiedName, Type QualifiedName)]
type UntypedDict = [(QualifiedName, Type QualifiedName)]

typeDictionary :: [RawDecl] -> UnifyT (Type QualifiedName) Check ([RawDecl], [(Type QualifiedName, RawDecl)], TypedDict, UntypedDict)
typeDictionary vals = do
  let values = sortOn valueName $ filter isValue vals
      sigs =  sortOn signatureName $ filter isSignature vals
      sigNames = map signatureName sigs
      (typedVals, untyped) = partition (\v -> valueName v `elem` sigNames) values
      typed = zip (map (signatureType) sigs) typedVals

  untypedNames <- replicateM (length untyped) (fresh)
  let untypedDict = zip (map valueName untyped) untypedNames
      typedDict   = map (\(t, v) -> (valueName v, t)) typed
  return (untyped, typed, typedDict ++ untypedDict, untypedDict)
  where
  valueName (_ :< Value n _) = n
  signatureName (_ :< Signature n _) = n
  signatureType (_ :< Signature _ t) = t

typeForBindingGroupEl :: RawDecl -> UntypedDict -> UnifyT (Type QualifiedName) Check (Decl QualifiedName TypedAnn)
typeForBindingGroupEl (a :< Value name els) dict = rethrow (ErrorInDecl name) $ do
  let (pats, _) = unzip els
      numArgs = length $ head pats
  when (any (/= numArgs) $ map length pats) . throwError $ InternalError "branches have different amounts of patterns"

  patTys <- replicateM numArgs fresh
  retTy <- fresh

  x <- forM els $ \(pats, val) -> do
    (patDict, pats') <- inferPats (zip patTys pats)
    (val', cons) <- runWriterT $ do
      val' <- bindNames dict $ bindNames patDict (infer val)
      typeOf val' =??= retTy
      return val'

    return ((pats', val'), cons)

  let (vals', cons') = unzip x
  let fTy = foldr tFn (typeOf . snd $ last vals') patTys
      memberType = fromJust (lookup name dict)

  fTy =?= memberType

  return $ Ann a (constrain (concat cons') memberType) :< Value name vals'

checkBindingGroupEl :: Type QualifiedName -> RawDecl -> TypedDict -> UnifyT (Type QualifiedName) Check (Decl QualifiedName TypedAnn)
checkBindingGroupEl ty (a :< Value name els) dict = rethrow (ErrorInDecl name) $ do
  let (pats, _) = unzip els
      numArgs = length $ head pats

  when (any (/= numArgs) $ map length pats) . throwError $ InternalError "branches have different amounts of patterns"

  let (constraints, ty') = unconstrained $ unForall ty
      unwrapped = unwrapN numArgs ty'
      argTys    = if length unwrapped > 1 then init unwrapped else []
      retTy     = last unwrapped
      unForall (Forall _ ty) = ty
      unForall ty = ty

  (cons, vals') <- liftM unzip . forM els $ \(pats, val) -> do
    let patTys = zip argTys pats

    (patDict, pats') <- inferPats patTys
    (val', cons) <- runWriterT $ do
      val' <- bindNames (patDict ++ dict) (check retTy val)
      retTy =??= typeOf val'
      return val'

    return (cons, (pats', val'))

  minCons <- UnifyT . lift $ reduce (concat cons)

  validateConstraints constraints minCons

  return $ Ann a ty :< Value name vals'

  where

  validateConstraints :: [Constraint QualifiedName] -> [Constraint QualifiedName] -> UnifyT (Type QualifiedName) Check ()
  validateConstraints given inferred = do
    sub <- unifyCurrentSubstitution <$> UnifyT get
    let subbed = concatMap (subCons sub) inferred

    UnifyT . lift $ checkSufficientConstraints given subbed


  subCons sub (n, tys) = let
    subbed = sub $? tys
    (consLists, baseTy) = unconstrained subbed
    in (n, baseTy) : consLists
