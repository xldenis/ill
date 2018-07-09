{-# LANGUAGE LambdaCase #-}
module Thrill.Desugar.Trait
( desugarTraits
)
where

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

import           Thrill.Prelude

import           Control.Lens      hiding ((:<))
import           Control.Monad.Reader

import           Data.Bifunctor
import           Data.Semigroup
import qualified Data.Map as M

import           Thrill.Desugar.Cases
import           Thrill.Infer.Entail  as E
import           Thrill.Infer.Monad hiding (env)
import           Thrill.Syntax
import           Thrill.Syntax.Pretty (pretty)
{-
  1. Handle super classes and superclass accessors
  2. Fix dictionary passing in dictionary definitions
-}

desugarTraits :: Environment -> Module QualifiedName TypedAnn -> Module QualifiedName TypedAnn
desugarTraits env (Module nm ds) = Module nm (fromDecl =<< ds)
  where
  fromDecl (_ :< TraitDecl supers nm arg members) = dataFromDecl supers nm arg members
  fromDecl (_ :< TraitImpl supers nm ty  members) = join $ runReaderT (valFromInst supers nm ty members) (fromEnv env)
  fromDecl (a :< Value name eqns) = runReaderT (addDictsToVals a name eqns) (fromEnv env)
  fromDecl x = pure x

data TraitDesugarEnv = TDEnv
  { env :: Environment
  , traitMethods :: [QualifiedName]
  }

fromEnv env = let
  methodNames = join $ map (map fst . methodSigs . snd) (traits env)
  in TDEnv env methodNames

{-
  Transform an instance declaration into a value of the dictionary type.
-}

valFromInst :: MonadReader TraitDesugarEnv m => [Constraint QualifiedName] -> QualifiedName -> Type QualifiedName -> [Decl QualifiedName TypedAnn] -> m [Decl QualifiedName TypedAnn]
valFromInst supers nm ty decls = do
  trait <- reader (lookup nm . traits . env) >>= \case
    Just traitDict -> return traitDict
    Nothing -> error "this isn't possible"

  let
    instName    = instanceName (nm, ty)
    memberTys = map (generalizeWithout [vars] . snd . unconstrained . snd) $ sortOn fst (methodSigs trait)
    superTys  = map mkDictType (superTraits trait)
    vars      = traitVarNm trait
    sortedDecls = sortOn valueName (map (prefixMemberNameWith instName) decls)

  memberVals <- forM sortedDecls $ \(a :< Value nm eqns) -> addDictsToVals a nm eqns

  let
    tyConTy   = mkDictConsType nm vars superTys memberTys
    subConTy  = applyTypeVars [ty] tyConTy
    tyCon       = TyAnn Nothing (Type tyConTy (Just subConTy)) :< Constructor (fmap ("Mk" <>) nm)
    dictTy      = mkDictType (nm, ty)
    memberReferences = map (variableFromMember (map TVar $ freeVariables dictTy)) sortedDecls
    applied     = SynAnn dictTy :< (Apply tyCon $ superDicts ++ (memberReferences))
    valAnn      = SynAnn $ constrain supers dictTy
    subbedSuperTraits = map (substituteConstraint [(vars, ty)]) (superTraits trait)
    superDicts  = map (\inst -> SynAnn (mkDictType inst) :< Var (instanceName inst)) subbedSuperTraits

  instVal <- addDictsToVals valAnn instName [([], applied)]

  return $ instVal : memberVals
  where

  substituteConstraint subst (nm, tys) = (nm, (replaceTypeVars subst) tys)

  prefixMemberNameWith :: QualifiedName -> Decl QualifiedName TypedAnn -> Decl QualifiedName TypedAnn
  prefixMemberNameWith pref (a :< v) =  a :< v { declName = mergeNames pref (declName v)}
  variableFromMember bv v = let
    ann = TyAnn Nothing (Type (generalize $ typeOf v) (Just $ applyTypeVars bv (typeOf v)))
    in ann :< Var (valueName v)

  valueName (_ :< Value nm _) = nm

  valueEqns (_ :< Value _ eqns) = eqns

  fromValue (_ :< Value _ [([], e)]) = e
  fromValue _ = error "impossible non-value during trait decl desugaring"

  unforall (Forall _ ty) = ty
  unforall ty = ty

instanceName :: Constraint QualifiedName -> QualifiedName
instanceName (nm, tys) = fmap (<> intercalate "_" (map qualName $ toList tys)) nm

mkProductType :: a -> [Type a] -> Type a
mkProductType nm args = foldl TAp (TConstructor nm) args

mkDictType :: (a, Type a) -> Type a
mkDictType (nm, args) = TAp (TConstructor nm) args

constraintsToFn :: Ord a => Bool -> Type a -> Type a
constraintsToFn needsGen ty = let
  (cons, fTy) = unconstrained ty
  vars = concat $ map (freeVariables . snd) cons
  baseTy = if needsGen then generalizeWithout vars fTy else fTy

  in foldr tFn baseTy $ (map mkDictType cons)

mkDictConsType :: Ord a => a -> a -> [Type a] -> [Type a] -> Type a
mkDictConsType name vars supers members = let
  builtDictTy = mkDictType (name, TVar vars)
  in generalize $ foldr tFn builtDictTy (supers ++ members)

{-
  Generate the datatype from a trait declaration. Also generates accessors for trait methods.
-}

dataFromDecl :: [Constraint QualifiedName] -> QualifiedName -> QualifiedName -> [Decl QualifiedName TypedAnn] -> [Decl QualifiedName TypedAnn]
dataFromDecl superTraits name vars members = let
  sortedMem = sortOn sigNm members
  dataRec   = (:<) (TyAnn Nothing (Kind dataKind)) . Data name [vars] . pure . mkProductType tyName
  dataKind  = Star `KFn` Star
  tyName    = fmap ("Mk" <>) name
  memTys    = map (generalizeWithout [vars] . typeOf) sortedMem
  recTy     = (TConstructor name) `TAp` TVar vars
  memNms    = map sigNm sortedMem
  superTys  = map mkDictType superTraits
  accessors = zipWith (mkAccessor tyName recTy (superTys ++ memTys)) memNms [(length superTys + 1)..]
  -- figure out the kind of new datatype
  in dataRec (superTys ++ memTys) : accessors
  where
  sigNm (_ :< Signature nm _) = nm

{-
  Rewrite values to explicitly pass around dictionaries.
-}

addDictsToVals :: MonadReader TraitDesugarEnv m => TypedAnn -> QualifiedName -> [Eqn TypedAnn] -> m (Decl QualifiedName TypedAnn)
addDictsToVals ann nm eqns = do
  instanceDicts <- reader (traitDictionaries . env)

  let
    (memberConstraints, _) = unconstrained (fromTyAnn ann)
    fty' = generalize $ constraintsToFn False (unforall $ fromTyAnn ann)
    instDictNames  = M.toList instanceDicts >>= instanceDictToConstraint >>= \i -> pure (i, Global $ instanceName i)
    localDictNames = zipWith (\cons ix -> (cons, Local . Internal $ "dict" ++ show ix)) memberConstraints [1..]
    instanceDict   = foldl (\x (nm, tys) -> M.insertWith (flip (++)) nm [InstanceEntry tys [] nm] x) instanceDicts memberConstraints
    dictPats       = map (\(cons, Local nm) -> SynAnn (mkDictType cons) :< PVar nm) localDictNames
    nameDict       = instDictNames ++ localDictNames

  local (\t -> t { env = (env t) { traitDictionaries = instanceDict } }) $ do
    eqns' <- addDictsToEqns nameDict eqns
    return $ ann { ty = Type fty' Nothing } :< Value nm (addPatsToEqns dictPats eqns')
  where
  instanceDictToConstraint :: (QualifiedName, [InstanceEntry]) -> [Constraint QualifiedName]
  instanceDictToConstraint (nm, instances) = map (\i -> (nm, instType i)) instances

  addPatsToEqns :: [Pat TypedAnn] -> [Eqn TypedAnn] -> [Eqn TypedAnn]
  addPatsToEqns ps eqns = map (first ((++) ps)) eqns

  unforall (Forall _ ty) = ty
  unforall ty = ty

data Dict a = Local a | Global a
  deriving (Show, Eq, Foldable)

type NameDict = [(Constraint QualifiedName, Dict QualifiedName)]

addDictsToEqns :: MonadReader TraitDesugarEnv m => NameDict -> [Eqn TypedAnn] -> m [Eqn TypedAnn]
addDictsToEqns dict eqns = forMOf (each . _2) eqns (transformMOf plate (replaceTraitMethods dict))

replaceTraitMethods :: MonadReader TraitDesugarEnv m => NameDict -> Expr TypedAnn -> m (Expr TypedAnn)
replaceTraitMethods dicts v@(a :< Var nm) = do
  case constraints (typeOf v) /= [] of
    True -> do
      let
        accessorType = generalize . constraintsToFn True . polyTy $ ty a
        accessorAnn  = Type accessorType (Just $ replaceTypeVars subst accessorType)
        subst        = getAnnSubst memberAnn
        memberAnn    = fmapTyAnn (snd . unconstrained) a
        traitCons    = constraints $ replaceTypeVars subst . polyTy $ ty a

      mkDictLookup memberAnn accessorAnn traitCons
    False -> pure v
   where

  fromJust' (Just x) = x
  fromJust' _ = error "fromJust in replaceTraitMethods"

  fmapTyAnn f ann = ann { ty = ( mapTy f (ty ann)) }
  mapTy f (Type pT iT) = Type (f pT) (f <$> iT)

  modifyPolyTy f (Type pT iT) = Type (f pT) iT

  mkDictLookup :: MonadReader TraitDesugarEnv m => TypedAnn -> TypeAnn -> [Constraint QualifiedName] ->  m (Expr TypedAnn)
  mkDictLookup memberAnn _ [] = return $ memberAnn :< Var nm
  mkDictLookup memberAnn accessorAnn cs = do
    id <- reader (traitDictionaries . env)
    memNames <- reader traitMethods

    let dictionaries = map (mkDictVal id) cs
    return $ case (dictionaries, nm `elem` memNames) of
      ([Global (a :< Var instName)], True) -> let
        entry = case findInst (head cs) id dicts of
          Just (_, entry, _) -> entry
          Nothing -> error "somehow i cant find this dict anymore!?"
        instanceMemberAnn = TyAnn Nothing (modifyPolyTy (generalize . applyTypeVars [instType entry]) (ty memberAnn))

        in instanceMemberAnn :< Var (fmap ((qualName instName) ++) nm)
      (dicts, _) -> memberAnn :< Apply (TyAnn Nothing accessorAnn :< Var nm) (toList =<< dicts)

  mkDictVal :: InstanceDict -> Constraint QualifiedName -> Dict (Expr TypedAnn)
  mkDictVal id traitCons = case findInst traitCons id dicts of
    Just (Local dict, _, _) -> Local (SynAnn (mkDictType traitCons) :< Var dict)
    Just (Global dict, entry, subTraits)   -> let
      dictAnn = instanceAnnotation traitCons entry subTraits
      dictVal = dictAnn :< Var dict
      in case subTraits of
        [] -> Global dictVal
        subTraits -> let
          dictionaries = toList . mkDictVal id =<< subTraits
          in Global $ SynAnn (mkDictType traitCons) :< Apply dictVal dictionaries

    Nothing -> error $ "couldnt find a dict for "
      ++ show (pretty traitCons) ++ "\n"
      ++ show (id)
      ++ show (findInst traitCons id dicts)

replaceTraitMethods _ x = pure x

instanceAnnotation :: Constraint QualifiedName -> InstanceEntry -> [Constraint QualifiedName] -> TypedAnn
instanceAnnotation c@(nm, ty) entry subDicts = let
  instTy = constraintsToFn False $ constrain subDicts (mkDictType c)
  polyTy = generalize $ constraintsToFn False $ constrain (instConstraints entry) polyDictTy
  polyDictTy = mkDictType (nm, instType entry)
  in TyAnn Nothing (Type polyTy $ Just instTy)

findInst :: Constraint QualifiedName -> InstanceDict -> NameDict -> Maybe (Dict QualifiedName, InstanceEntry, [Constraint QualifiedName])
findInst c@(nm, tys) id namedict = do
  (entry, subDict) <- matchInst id c

  -- this is currently very hacky... need to find a better way to represent this...
  matched <- find (matcher c) namedict
  return (snd matched, entry, subDict)
  where
  matcher c1         (c2, Local _) = c1 == c2
  matcher (nm1, ty1) ((nm2, ty2), _) | nm1 == nm2 = isJust $ subsume ty2 ty1
  matcher _ _ = False

mkAccessor :: QualifiedName -> Type QualifiedName -> [Type QualifiedName] -> QualifiedName -> Int -> Decl QualifiedName TypedAnn
mkAccessor tyNm recTy tys fldNm ix = let
  val = SynAnn valTy :< Var (Internal "el")
  valTy = tys !! (ix - 1)
  accessorTy = generalize $ recTy `tFn` valTy
  in SynAnn accessorTy :< Value fldNm [([mkPattern recTy tyNm tys ix], val)]

mkPattern :: Type QualifiedName -> QualifiedName -> [Type QualifiedName] -> Int -> Pat TypedAnn
mkPattern recTy recNm tys ix = let
  prePats  = replicate (ix - 1) Wildcard
  postPats = replicate (length tys - ix) Wildcard
  focused  = pure $ PVar (Internal "el")
  annPats   = zipWith (:<) (map SynAnn tys) $ prePats <> focused <> postPats
  in SynAnn recTy :< Destructor recNm annPats
