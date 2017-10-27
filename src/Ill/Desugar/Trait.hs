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


import           Control.Lens      hiding ((:<))
import           Control.Monad.Reader
import           Control.Comonad

import           Data.Bifunctor
import           Data.Foldable     (find, foldl', toList)
import           Data.List         (intercalate)
import           Data.Maybe
import           Data.Semigroup

import           Ill.Desugar.Cases
import           Ill.Infer.Entail  as E
import           Ill.Infer.Monad
import           Ill.Syntax
import           Ill.Syntax.Pretty (pretty)
{-
  1. Ensure Record is built in correct order
  2. Handle super classes and superclass accessors
  3. Fix dictionary passing in dictionary definitions
-}

desugarTraits :: Environment -> [Decl TypedAnn] -> [Decl TypedAnn]
desugarTraits env ds = fromDecl =<< ds
  where
  fromDecl (_ :< TraitDecl supers nm args members) = dataFromDecl supers nm args members
  fromDecl (_ :< TraitImpl supers nm tys  members) = runReaderT (valFromInst supers nm tys members) env
  fromDecl (a :< Value name eqns) = runReaderT (addDictsToVals a name eqns) env
  fromDecl x = pure x

valFromInst :: MonadReader Environment m => [Constraint Name] -> Name -> [Type Name] -> [Decl TypedAnn] -> m (Decl TypedAnn)
valFromInst supers nm tys decls = do
  let
    tyCon    = SynAnn tyConTy :< Constructor ("Mk" <> nm)
    tyConTy  = foldl tFn (mkDictType nm tys) (map typeOf decls)
    instName = instanceName (nm, tys)
    dictTy   = mkAnn $ mkDictType nm tys
    applied  = dictTy :< Apply tyCon (map (fromValue . simplifyPatterns) decls)
    valAnn    = mkAnn $ constrain supers (mkDictType nm tys)
    superDicts = map (uncurry mkDictType) supers

  addDictsToVals valAnn instName [([], applied)]
  where

  valueName (_ :< Value nm _) = nm

  valueEqns (_ :< Value _ eqns) = eqns

  fromValue (_ :< Value _ [([], e)]) = e
  fromValue _ = error "impossible non-value during trait decl desugaring"

instanceName (nm, tys) = nm <> intercalate "_" (toList =<< tys)
mkDictType nm args = foldl TAp (TConstructor nm) args

constraintsToFn ty = let (cons, fTy) = unconstrained ty
  in foldr1 tFn $ (map (uncurry mkDictType) cons) ++ unwrapFnType fTy

dataFromDecl :: [Constraint Name] -> Name -> [Name] -> [Decl TypedAnn] -> [Decl TypedAnn]
dataFromDecl superTraits name vars members = let
  dataRec   = (:<) (TyAnn Nothing (Kind dataKind)) . Data name vars . pure . mkDictType tyName
  dataKind  = foldl (\c _ -> c `KFn` Star) Star vars
  tyName    = "Mk" <> name
  memTys    = map typeOf members
  recTy     = foldl (\l r -> l `TAp` TVar r) (TConstructor name) vars
  memNms    = map sigNm members
  superTys  = []
  accessors = zipWith (mkAccessor tyName recTy (memTys ++ superTys)) memNms [1..length members]
  -- figure out the kind of new datatype
  in dataRec (superTys ++ memTys) : accessors
  where
  sigNm (_ :< Signature nm _) = nm

addDictsToVals :: MonadReader Environment m => TypedAnn -> Name -> [Eqn TypedAnn] -> m (Decl TypedAnn)
addDictsToVals ann nm eqns = do
  instanceDicts <- reader traitDictionaries

  let
    (memberConstraints, memberTy) = unconstrained (fromTyAnn ann)
    fty' = constraintsToFn (fromTyAnn ann)
    instDictNames = instanceDicts >>= instanceDictToConstraint >>= \i -> pure (i, instanceName i)
    localNameDict = zipWith (\cons ix -> (cons, "dict" ++ show ix)) memberConstraints [1..]
    localInstances = map (\(nm, tys) -> case nm `lookup` instanceDicts of
      Just instances -> (nm, instances ++ [(tys, [])])
      Nothing        -> (nm, [(tys, [])])
      ) memberConstraints
    instanceDict = foldr addInstanceToDict instanceDicts localInstances
    dictPats = map (\(cons, nm) -> SynAnn (uncurry mkDictType cons) :< PVar nm) localNameDict
    nameDict = instDictNames ++ localNameDict

  local (\env -> env { traitDictionaries = instanceDict }) $ do
    eqns' <- addDictsToEqns nameDict eqns
    return $ ann { ty = Type fty' } :< Value nm (addPatsToEqns dictPats $ eqns')
  where
  instanceDictToConstraint :: (Name, [TraitInstance]) -> [Constraint Name]
  instanceDictToConstraint (nm, instances) = map (\i -> (nm, fst i)) instances

  addPatsToEqns ps eqns = map (first ((++) ps)) eqns

type NameDict = [(Constraint Name, Name)]

addDictsToEqns :: MonadReader Environment m => NameDict -> [Eqn TypedAnn] -> m [Eqn TypedAnn]
addDictsToEqns dict eqns = forMOf (each . _2) eqns (transformM (replaceTraitMethods dict))

replaceTraitMethods :: MonadReader Environment m => NameDict -> Expr TypedAnn -> m (Expr TypedAnn)
replaceTraitMethods dicts v@(a :< Var nm) = do
  allTraitMethods <- reader traits >>= pure . join . map (methodSigs . snd)
  case nm `lookup` allTraitMethods of
    Just x  -> do
      instanceDict <- reader traitDictionaries
      return $ mkDictLookup instanceDict x
    Nothing -> pure v

  where
  mkDictLookup id ty = a :< Apply (SynAnn ty :< Var nm) (map (mkDictVal id) (constraints $ typeOf v))
  mkDictVal id ty = case findInst ty id dicts of
      Just (dict, [])   -> SynAnn tNil :< Var dict
      Just (dict, cons) -> SynAnn tNil :< Apply (SynAnn tNil :< Var dict) (map (mkDictVal id) cons)
      Nothing -> error $ "couldnt find a dict for "
        ++ show (pretty ty)
        ++ show (pretty id)
        ++ show (findInst ty id dicts)
replaceTraitMethods _ x = pure x

findInst :: Constraint Name -> InstanceDict -> NameDict -> Maybe (Name, [Constraint Name])
findInst c@(nm, tys) id namedict = do
  subDict <- goalsByInst id c
  matched <- find (\(c2, _) -> matcher c c2) namedict
  return (snd matched, subDict)
  where
  consTy = foldl1 TAp
  matcher (nm1, ty1) (nm2, ty2) | nm1 == nm2 = isJust $ E.match (consTy ty2) (consTy ty1)
  matcher _ _ = False

mkAccessor tyNm recTy tys fldNm ix = let
  val = mkAnn valTy :< Var "el"
  valTy = tys !! (ix - 1)
  accessorTy = recTy `tFn` valTy
  in mkAnn accessorTy :< Value fldNm [([mkPattern recTy tyNm tys ix], val)]

mkPattern :: Type Name -> Name -> [Type Name] -> Int -> Pat TypedAnn
mkPattern recTy recNm tys ix = let
  prePats  = replicate (ix - 1) Wildcard
  postPats = replicate (length tys - ix) Wildcard
  focused  = pure $ PVar "el"
  annPats   = zipWith (:<) (map SynAnn tys) $ prePats <> focused <> postPats
  in mkAnn recTy :< Destructor recNm annPats

mkAnn = SynAnn
{-

Given MkShow (A a) b c

fn ShowA (MkShow a _ _)
fn b (MkShow _ b _)
-}

valueFromInst :: [Constraint Name] -> Name -> [Type Name] -> [Decl TypedAnn] -> Decl TypedAnn
valueFromInst = error "todo"

