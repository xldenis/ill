{-# LANGUAGE LambdaCase #-}
module Ill.Desugar.Trait where

{-

Implements trait desugaring

This consists of 3 different things

1. Translate trait definitions to trait dictionaries
2. Translate trait instances to dictionary values
3. Turn all trait method calls into dictionary lookups
  3 b. A future version may push this into typechecking for performance reasons
4. Extend all methods with dictionary arguments if they are polymorphic over a constrained type

Given a trait definition like:

trait Show a
  fn show :: a -> String
end

this will produce:

data Show a = MkShow (a -> String)

fn show (MkShow s) = s

Similarily an instance for Show Int:

impl Show Int
  fn show(1)
    '1'
  end
end

would be turned into:

showInt = MkShow (fn (1) =
  '1'
end)

A monomorphic usage of show will be simplified into a direct lookup of the correct dictionary:

fn x()
  show(1)
end

will be turned into:

fn x()
  show(showInt)(1)
end

A polymorphic usage of `show` will add a dictionary argument to the surrounding method

fn x()
  show
end

turns into:

fn x(dShow)
  show(dShow)
end

Super Traits
============

Super traits are desugared as nested dictionaries.

trait A b | B b
  b :: String
end

turns into:

data B b = MkB (A b) String

The rest of the desugaring proceeds as usual
-}

import           Ill.Prelude

import           Control.Lens      hiding ((:<))
import           Control.Monad.Reader

import           Data.Bifunctor
import           Data.Semigroup
import qualified Data.Map as M

import           Ill.Desugar.Cases
import           Ill.Infer.Entail  as E
import           Ill.Infer.Monad
import           Ill.Syntax
import           Ill.Syntax.Pretty (pretty)

import           Debug.Trace
{-
  1. Handle super classes and superclass accessors
  2. Fix dictionary passing in dictionary definitions
-}

desugarTraits :: Environment -> [Decl TypedAnn] -> [Decl TypedAnn]
desugarTraits env ds = fromDecl =<< ds
  where
  fromDecl (_ :< TraitDecl supers nm arg members) = dataFromDecl supers nm arg members
  fromDecl (_ :< TraitImpl supers nm ty  members) = runReaderT (valFromInst supers nm ty members) env
  fromDecl (a :< Value name eqns) = runReaderT (addDictsToVals a name eqns) env
  fromDecl x = pure x

valFromInst :: MonadReader Environment m => [Constraint Name] -> Name -> Type Name -> [Decl TypedAnn] -> m (Decl TypedAnn)
valFromInst supers nm ty decls = do
  trait <- reader traits >>= \traits -> case lookup nm traits of
    Just traitDict -> return traitDict
    Nothing -> error "this isn't possible"

  let memberTys = map (generalizeWithout [vars] . snd . unconstrained . snd) $ sortOn fst (methodSigs trait)
      superTys  = map (uncurry mkDictType) (superTraits trait)
      argVars   = nub $ freeVariables ty
      vars      = traitVar trait
      tyConTy   = mkDictConsType nm vars superTys memberTys
      subConTy  = applyTypeVars [ty] tyConTy
      subst     = [(vars, ty)]
  let
    sortedDecls = sortOn valueName decls
    tyCon       = TyAnn Nothing (Type tyConTy (Just subConTy)) :< Constructor ("Mk" <> nm)
    instName    = instanceName (nm, ty)
    dictTy      = mkDictType nm ty
    applied     = mkAnn dictTy :< ( Apply tyCon $ superDicts ++ (map (fromValue . simplifyPatterns) sortedDecls))
    valAnn      = mkAnn $ constrain supers dictTy
    superDictTy = map (SynAnn . uncurry mkDictType . substituteConstraint subst) (superTraits trait)
    superDictVal = map (Var . instanceName . substituteConstraint subst) (superTraits trait)
    superDicts  = zipWith (:<) superDictTy superDictVal

  -- there's a bug in simplify patterns its not returning the correct values
  addDictsToVals valAnn instName [([], applied)]
  where

  substituteConstraint subst (nm, tys) = (nm, (replaceTypeVars subst) tys)

  valueName (_ :< Value nm _) = nm

  valueEqns (_ :< Value _ eqns) = eqns

  fromValue (_ :< Value _ [([], e)]) = e
  fromValue _ = error "impossible non-value during trait decl desugaring"

  unforall (Forall _ ty) = ty
  unforall ty = ty

instanceName (nm, tys) = nm <> intercalate "_" (toList tys)

mkProductType :: a -> [Type a] -> Type a
mkProductType nm args = foldl TAp (TConstructor nm) args

mkDictType :: a -> Type a -> Type a
mkDictType nm args = TAp (TConstructor nm) args

constraintsToFn :: Bool -> Type Name -> Type Name
constraintsToFn needsGen ty = let
  (cons, fTy) = unconstrained ty
  vars :: [Name]
  vars = concat $ map (freeVariables . snd) cons
  baseTy = if needsGen then generalizeWithout vars fTy else fTy

  in foldr tFn baseTy $ (map (uncurry mkDictType) cons)

mkDictConsType :: Name -> Name -> [Type Name] -> [Type Name] -> Type Name
mkDictConsType name vars supers members = let
  builtDictTy = mkDictType name (TVar vars)
  in generalize $ foldr tFn builtDictTy (supers ++ members)

dataFromDecl :: [Constraint Name] -> Name -> Name -> [Decl TypedAnn] -> [Decl TypedAnn]
dataFromDecl superTraits name vars members = let
  sortedMem = sortOn sigNm members
  dataRec   = (:<) (TyAnn Nothing (Kind dataKind)) . Data name [vars] . pure . mkProductType tyName
  dataKind  = foldl (\c _ -> c `KFn` Star) Star vars
  tyName    = "Mk" <> name
  memTys    = map (generalizeWithout [vars] . typeOf) sortedMem
  recTy     = (TConstructor name) `TAp` TVar vars
  memNms    = map sigNm sortedMem
  superTys  = map (uncurry mkDictType) superTraits
  accessors = zipWith (mkAccessor tyName recTy (superTys ++ memTys)) memNms [(length superTys + 1)..]
  -- figure out the kind of new datatype
  in dataRec (superTys ++ memTys) : accessors
  where
  sigNm (_ :< Signature nm _) = nm

addDictsToVals :: MonadReader Environment m => TypedAnn -> Name -> [Eqn TypedAnn] -> m (Decl TypedAnn)
addDictsToVals ann nm eqns = do
  instanceDicts <- reader traitDictionaries

  let
    (memberConstraints, memberTy) = unconstrained (fromTyAnn ann)
    fty' = generalize $ constraintsToFn False (unforall $ fromTyAnn ann)
    instDictNames = M.toList instanceDicts >>= instanceDictToConstraint >>= \i -> pure (i, GlobalDict $ instanceName i)
    localNameDict = zipWith (\cons ix -> (cons, LocalDict $ "dict" ++ show ix)) memberConstraints [1..]
    instanceDict = foldl (\x (nm, tys) -> M.insertWith (flip (++)) nm [InstanceEntry tys [] nm] x) instanceDicts memberConstraints
    dictPats = map (\(cons, LocalDict nm) -> SynAnn (uncurry mkDictType cons) :< PVar nm) localNameDict
    nameDict = instDictNames ++ localNameDict

  local (\env -> env { traitDictionaries = instanceDict }) $ do
    eqns' <- addDictsToEqns nameDict eqns
    return $ ann { ty = Type fty' Nothing } :< Value nm (addPatsToEqns dictPats $ eqns')
  where
  instanceDictToConstraint :: (Name, [InstanceEntry]) -> [Constraint Name]
  instanceDictToConstraint (nm, instances) = map (\i -> (nm, instType i)) instances

  addPatsToEqns ps eqns = map (first ((++) ps)) eqns

  unforall (Forall _ ty) = ty
  unforall ty = ty

data DictName = LocalDict Name | GlobalDict Name
  deriving (Show, Eq)
type NameDict = [(Constraint Name, DictName)]

addDictsToEqns :: MonadReader Environment m => NameDict -> [Eqn TypedAnn] -> m [Eqn TypedAnn]
addDictsToEqns dict eqns = forMOf (each . _2) eqns (transformMOf plate (replaceTraitMethods dict))

{-
  The intent of this method is to provide a top-down traversal that provides a dict of bound names along the way

-}

transformMOfWithNames l f = go where
  go t@(_ :< Assign lhs rhs) = do
    mapMOf l go t >>= f
  go t = mapMOf l go t >>= f

replaceTraitMethods :: MonadReader Environment m => NameDict -> Expr TypedAnn -> m (Expr TypedAnn)
replaceTraitMethods dicts v@(a :< Var nm) = do
  traitInfo <- reader traits
  let allTraitMethods = join $ map (\(nm, entry) -> map (fmap (const(nm, entry))) (methodSigs entry)) traitInfo
  case nm `lookup` allTraitMethods of
    Just (traitNm, entry) -> do
      instanceDict <- reader traitDictionaries
      let traitCons   = constraints $ fromJust' $ instTyOf v
          instanceTys = fromJust' $ traitNm `lookup` traitCons
          memPolyTy   = generalize . constraintsToFn True . polyTy $ ty a
          tyAnn       = Type memPolyTy (Just $ applyTypeVars [instanceTys] memPolyTy)
          dictAnn     = fmapTyAnn (snd . unconstrained . applyTypeVars [instanceTys]) a
          dictVal     = dictAnn :< mkDictLookup instanceDict (tyAnn) (traitNm, instanceTys)

      return $ dictVal
    Nothing -> pure v

  where

  fromJust' (Just x) = x
  fromJust' _ = error "fromJust in replaceTraitMethods"

  fmapTyAnn f ann = ann { ty = ( mapTy f (ty ann)) }
  mapTy f (Type pT iT) = Type (f pT) (f <$> iT)

  mkDictLookup :: InstanceDict -> TypeAnn -> Constraint Name -> Expression TypedAnn (Expr TypedAnn)
  mkDictLookup id ty c = Apply (TyAnn Nothing ty :< Var nm) [mkDictVal id c]

  mkDictVal :: InstanceDict -> Constraint Name -> Expr TypedAnn
  mkDictVal id traitCons = case findInst traitCons id dicts of
    Just (LocalDict dict, _, _) -> SynAnn instDictTy :< Var dict
    Just (GlobalDict dict, entry, subTraits)   -> let
      freeVars = nub $ freeVariables (instType entry)
      consVars = concat $ fmap (freeVariables . snd) (instConstraints entry)
      instTy = constraintsToFn False $ constrain (subTraits) instDictTy
      polyTy = generalize $ constraintsToFn False $ constrain (instConstraints entry) polyDictTy
      polyDictTy = mkDictType (fst traitCons) (instType entry)
      dictAnn = TyAnn Nothing (Type polyTy $ Just instTy)
      in case subTraits of
        [] -> dictAnn :< Var dict
        subTraits -> SynAnn (uncurry mkDictType traitCons) :< Apply (dictAnn :< Var dict) (map (mkDictVal id) subTraits)

    Nothing -> error $ "couldnt find a dict for "
      ++ show (pretty traitCons) ++ "\n"
      ++ show (id)
      ++ show (findInst traitCons id dicts)
    where
    instDictTy = uncurry mkDictType traitCons

replaceTraitMethods _ x = pure x

findInst :: Constraint Name -> InstanceDict -> NameDict -> Maybe (DictName, InstanceEntry, [Constraint Name])
findInst c@(nm, tys) id namedict = do
  (entry, subDict) <- matchInst id c
  matched <- find (\(c2, _) -> matcher c c2) namedict
  return (snd matched, entry, subDict)
  where
  matcher (nm1, ty1) (nm2, ty2) | nm1 == nm2 = isJust $ subsume (ty2) (ty1)
  matcher _ _ = False

mkAccessor tyNm recTy tys fldNm ix = let
  val = mkAnn valTy :< Var "el"
  valTy = tys !! (ix - 1)
  accessorTy = generalize $ recTy `tFn` valTy
  in mkAnn accessorTy :< Value fldNm [([mkPattern recTy tyNm tys ix], val)]

mkPattern :: Type Name -> Name -> [Type Name] -> Int -> Pat TypedAnn
mkPattern recTy recNm tys ix = let
  prePats  = replicate (ix - 1) Wildcard
  postPats = replicate (length tys - ix) Wildcard
  focused  = pure $ PVar "el"
  annPats   = zipWith (:<) (map SynAnn tys) $ prePats <> focused <> postPats
  in mkAnn recTy :< Destructor recNm annPats

mkAnn = SynAnn
