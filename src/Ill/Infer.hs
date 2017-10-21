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
import Debug.Trace

type RawDecl = Decl SourceSpan

{-
  1. kind checking not implemented
  2. error messages suuuuuck
  3. Fail assign terminated blocks during type checking
  4. Capture avoidance during typechecking!
-}

typeCheck :: [BindingGroup SourceSpan] -> Check [BindingGroup TypedAnn]
typeCheck bgs = mapM go bgs
  where
  go :: BindingGroup SourceSpan -> Check (BindingGroup TypedAnn)
  go (ValueBG ds)                  = do
    inferredVals <- liftUnify $ do
          (ut, et, dict, untypedDict) <- typeDictionary ds
          explicit <- forM et $ \e -> uncurry checkBindingGroupEl e dict
          implicit <- forM ut $ \e -> typeForBindingGroupEl e dict

          return $ explicit ++ implicit

    subbedValues <- appSubs inferredVals
    values <- forM subbedValues $ \(ann :< v) -> do
      let t = fromTyAnn ann

      t' <- flattenConstraints <$> simplify' t
      when (ambiguous t') $ internalError $ "ambigious type!!!!" ++ show (ambiguities $ t')

      let generalizedType = generalize t'
      addValue (valueName v) generalizedType
      return $ (ann { ty = Type generalizedType }) :< v

    return $ ValueBG values

    where

    valueName (Value n _) = n

    appSubs :: ([Decl TypedAnn], Substitution (Type Name)) -> Check [Decl TypedAnn]
    appSubs (ts, sub) = mapM (substituteOneDecl sub) ts

    substituteOneDecl :: Substitution (Type Name) -> Decl TypedAnn -> Check (Decl TypedAnn)
    substituteOneDecl sub decl = hoistAppToCofree (substituteAnn sub) $ decl

    substituteAnn :: Substitution (Type Name) -> TypedAnn -> Check (TypedAnn)
    substituteAnn sub = \ann -> do
      let annTy = fromTyAnn ann
          subbedType = substituteOneType sub annTy
      simplified <- pure subbedType
      pure $ ann { ty = Type (simplified) }

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
    consPair   = fromCons . fromJust . uncons . reverse . unfoldCons

    fromCons (TConstructor n, b) = (n,b)
    fromCons _ = error "non constructor value found"

    unfoldCons (TAp f a) = a : unfoldCons f
    unfoldCons a         = [a]

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
        tySubs     = zip (traitVars trait) args
        sigTys     = map (fmap (constrain constraints' . replaceTypeVars tySubs)) (methodSigs trait)
        signatures = map (uncurry Signature) sigTys
        annotated  = map (\sig -> emptySpan :< sig) signatures

    vals' <- liftUnify $ do
      (ut, et, dict, untypedDict) <- typeDictionary (annotated ++ ds)

      when (not (null ut) || not (null untypedDict)) $ internalError "there are implicitly typed members to trait impl"

      withTraitInstance nm supers args $
        forM et $ \e -> uncurry checkBindingGroupEl e dict

    addTraitInstance nm supers args

    values <- forM (appSubs vals') $ \(ann :< v) -> do
      let t = varIfUnknown $ fromType (ty ann)

      t' <- flattenConstraints <$> simplify' t

      return $ (ann { ty = Type t' }) :< v


    return . OtherBG $ TyAnn (pure a) None :< TraitImpl supers nm args (filter isValue values)

    where
    traitName (Constrained _ t) = traitName t
    traitName (TAp f _)         = traitName f
    traitName (TConstructor c)  = c

    appSubs (ts, sub) = map (nestedFmap (\a -> a { ty = fmapTy (\v -> varIfUnknown $ sub $? v) (ty a) })) ts

    simplify' (Constrained cons t) = do
      cons' <- reduce cons
      return $ Constrained cons' t
    simplify' (Forall vars ty) = Forall vars <$> simplify' ty

    simplify' t = return t

    emptySpan = SourceSpan (initialPos "") (initialPos "")

  go (OtherBG _)                 = throwError $ NotImplementedError "oops"

type TypedDict   = [(Name, Type Name)]
type UntypedDict = [(Name, Type Name)]

typeDictionary :: [Decl SourceSpan] -> UnifyT (Type Name) Check ([Decl SourceSpan], [(Type Name, RawDecl)], TypedDict, UntypedDict)
typeDictionary vals = do
  let values = sortOn valueName $ filter isValue vals
      sigs =  sortOn signatureName $ filter isSignature vals
      sigNames = map signatureName sigs
      (typedVals, untyped) = partition (\v -> valueName v `elem` sigNames) values
      typed = zip (map (generalize . signatureType) sigs) typedVals

  untypedNames <- replicateM (length untyped) (fresh)
  let untypedDict = zip (map valueName untyped) untypedNames
      typedDict   = map (\(t, v) -> (valueName v, t)) typed
  return (untyped, typed, typedDict ++ untypedDict, untypedDict)
  where
  valueName (_ :< Value n _) = n
  signatureName (_ :< Signature n _) = n
  signatureType (_ :< Signature _ t) = t

typeForBindingGroupEl :: RawDecl -> UntypedDict -> UnifyT (Type Name) Check (Decl TypedAnn)
typeForBindingGroupEl (a :< Value name els) dict = do
  let (pats, _) = unzip els
      numArgs = length $ head pats
  when (any (/= numArgs) $ map length pats) . throwError $ InternalError "branches have different amounts of patterns"

  patTys <- replicateM numArgs fresh
  retTy <- fresh

  x <- forM els $ \(pats, val) -> do
    (patDict, pats') <- inferPats (zip patTys pats)
    val' <- bindNames (patDict ++ dict) (infer val)

    cons <- typeOf val' `constrainedUnification` retTy
    return ((pats', val'), cons)

  let (vals', cons') = unzip x
  let fTy = foldr tFn (typeOf . snd $ last vals') patTys
      memberType = fromJust (lookup name dict)

  fTy `constrainedUnification` memberType

  return $ Ann a (constrain (concat cons') memberType) :< Value name vals'

checkBindingGroupEl :: Type Name -> RawDecl -> TypedDict -> UnifyT (Type Name) Check (Decl TypedAnn)
checkBindingGroupEl ty (a :< Value name els) dict = do
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
