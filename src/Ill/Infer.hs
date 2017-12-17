module Ill.Infer
( module Ill.Infer
, runCheck
, defaultCheckEnv
, Environment(..)
, ConstructorEntry(..)
, TraitEntry(..)
, CheckState(..)
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

import           Ill.Prelude

import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Unify

import           Data.Coerce
import           Data.List
import           Data.Map             as M (union)
import           Data.Maybe

import           Text.Megaparsec      (initialPos)

import           Ill.BindingGroup
import           Ill.Error

import           Ill.Infer.Entail
import           Ill.Infer.Kind
import           Ill.Infer.Monad
import           Ill.Infer.Types

import           Ill.Parser.Lexer     (SourceSpan (..))
import           Ill.Syntax
import           Ill.Syntax.Type
import Data.Bitraversable
import Control.Lens.Plated

type RawDecl = Decl SourceSpan

{-
  1. kind checking not implemented
  2. error messages suck
-}

typeCheck :: BoundModules SourceSpan -> Check [BindingGroup TypedAnn]
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

  go :: BindingGroup SourceSpan -> Check (BindingGroup TypedAnn)
  go (ValueBG ds)                  = do
    inferredVals <- liftUnify $ do
      (ut, et, dict, untypedDict) <- typeDictionary (map generalizeSig ds)
      explicit <- forM et $ \e -> uncurry checkBindingGroupEl e dict
      implicit <- forM ut $ \e -> typeForBindingGroupEl e dict

      return $ explicit ++ implicit

    subbedValues <- appSubs inferredVals
    values <- forM subbedValues $ \(ann :< v) -> do
      let t = fromTyAnn ann

      t' <- flattenConstraints <$> simplify' t
      when (ambiguous t') $ internalError $ "ambigious type: " ++ show (ambiguities $ t') ++ valueName v ++ (show $ pretty (fromTyAnn ann))

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
      fvInConstraints = nub $ concatMap ((concatMap freeVariables) . snd ) constraints

    substituteOneType sub ty = varIfUnknown $ sub $? ty

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

    coerceAnn :: Declaration SourceSpan (Decl SourceSpan) -> Declaration TypedAnn (Decl TypedAnn)
    coerceAnn (Data n vars cons) = Data n vars cons
    coerceAnn _ = error "impossible non-data value found in data binding group"

    consPair :: Type Name -> (Name, [Type Name])
    consPair   = fromCons . fromJust . uncons . unwrapProduct

    fromCons (TConstructor n, b) = (n,b)
    fromCons _ = error "non constructor value found"

  go (OtherBG (_ :< TypeSynonym{})) = throwError $ NotImplementedError "oops"
  go (OtherBG (a :< Import q m n al)) = return $ OtherBG $ TyAnn (pure a) None :< Import q m n al
  go (OtherBG (a :< TraitDecl supers name args members)) = do
    let memTys = map toPair members
        members' = map annSigs members

    addTrait name supers args memTys

    return . OtherBG $ TyAnn (pure a) None :< TraitDecl supers name args members'
    where
    toPair (_ :< Signature nm ty) = (nm, ty)
    toPair _ = error "trait declaration contains non signature value"

    annSigs (a :< Signature nm ty) = Ann a ty :< Signature nm ty
    annSigs _ = error "trait declaration contains non signature value"

  go (OtherBG (a :< TraitImpl supers nm args ds)) = do
    trait <- lookupTrait nm
    let subs = zip (traitVars trait) args
        cons = map (\(nm, cs') -> (nm, map (replaceTypeVars subs) cs' )) (superTraits trait)

    unsatisfiedSupers <- reduce cons
    when (not $ null unsatisfiedSupers) . internalError $ "unsatisfied supertraits in instance: " ++ show unsatisfiedSupers

    let constraints' = (nm, args) : supers
        argVars    = nub . concat $ map freeVariables args
        sigTys     = map (fmap (generalizeWithout argVars . constrain constraints' . applyTypeVars args)) (methodSigs trait)
        signatures = map (uncurry Signature) sigTys
        annotated  = map (\sig -> emptySpan :< sig) signatures

    vals' <- liftUnify $ do
      (ut, et, dict, untypedDict) <- typeDictionary (annotated ++ ds)

      when (not (null ut) || not (null untypedDict)) $ do
        internalError . intercalate "\n" $ [ "The trait " ++ nm ++ " does not contain the methods:" ] ++ map valueName ut

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

appSubs :: ([Decl TypedAnn], Substitution (Type Name)) -> Check [Decl TypedAnn]
appSubs (ts, sub) = mapM (substituteOneDecl sub) ts

substituteOneDecl :: Substitution (Type Name) -> Decl TypedAnn -> Check (Decl TypedAnn)
substituteOneDecl sub decl = hoistAppToCofree (substituteAnn sub) $ decl

substituteAnn :: Substitution (Type Name) -> TypedAnn -> Check (TypedAnn)
substituteAnn sub = \ann -> do
  let (Type poly inst) = ty ann
      subbedPoly = substituteOneType sub poly
      subbedInst = substituteOneType sub <$> inst

  pure $ ann { ty = Type subbedPoly subbedInst }

substituteOneType :: Substitution (Type Name) -> Type Name -> Type Name
substituteOneType sub ty = varIfUnknown $ sub $? ty

type TypedDict   = [(Name, Type Name)]
type UntypedDict = [(Name, Type Name)]

typeDictionary :: [Decl SourceSpan] -> UnifyT (Type Name) Check ([Decl SourceSpan], [(Type Name, RawDecl)], TypedDict, UntypedDict)
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

typeForBindingGroupEl :: RawDecl -> UntypedDict -> UnifyT (Type Name) Check (Decl TypedAnn)
typeForBindingGroupEl (a :< Value name els) dict = rethrow (ErrorInDecl name) $ do
  let (pats, _) = unzip els
      numArgs = length $ head pats
  when (any (/= numArgs) $ map length pats) . throwError $ InternalError "branches have different amounts of patterns"

  patTys <- replicateM numArgs fresh
  retTy <- fresh

  x <- forM els $ \(pats, val) -> do
    (patDict, pats') <- inferPats (zip patTys pats)
    val' <- bindNames dict $ bindNames patDict (infer val)

    cons <- typeOf val' `constrainedUnification` retTy
    return ((pats', val'), cons)

  let (vals', cons') = unzip x
  let fTy = foldr tFn (typeOf . snd $ last vals') patTys
      memberType = fromJust (lookup name dict)

  fTy `constrainedUnification` memberType

  return $ Ann a (constrain (concat cons') memberType) :< Value name vals'

checkBindingGroupEl :: Type Name -> RawDecl -> TypedDict -> UnifyT (Type Name) Check (Decl TypedAnn)
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
    val' <- bindNames (patDict ++ dict) (check retTy val)
    cons <- retTy `constrainedUnification` typeOf val'

    return (cons, (pats', val'))

  minCons <- UnifyT . lift $ reduce (concat cons)

  validateConstraints constraints minCons

  return $ Ann a ty :< Value name vals'

  where

  validateConstraints :: [Constraint Name] -> [Constraint Name] -> UnifyT (Type Name) Check ()
  validateConstraints given inferred = do
    sub <- unifyCurrentSubstitution <$> UnifyT get
    let subbed = map (subCons sub) inferred

    UnifyT . lift $ checkSufficientConstraints given subbed


  subCons sub (n, tys) = (n, map (sub $?) tys)
