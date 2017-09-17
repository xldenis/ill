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

import Data.Semigroup
import Ill.Syntax
import Ill.Desugar.Cases
import Data.List (intercalate)
import Data.Foldable (toList, foldl')

todo = error "todo"


{-
  1. Ensure Record is built in correct order
  2. Handle super classes and superclass accessors
  3. InstanceNaming
-}


desugarTraits :: [Decl TypedAnn] -> [Decl TypedAnn]
desugarTraits ds = fromDecl =<< ds
  where
  fromDecl (_ :< TraitDecl supers nm args members) = dataFromDecl supers nm args members
  fromDecl (_ :< TraitImpl supers nm tys  members) = pure $ valFromInst supers nm tys members
  fromDecl x = pure x

valFromInst :: [Constraint Name] -> Name -> [Type Name] -> [Decl TypedAnn] -> Decl TypedAnn
valFromInst supers nm tys decls = let
  members = map simplifyPatterns decls
  tyCon = SynAnn tyConTy :< Constructor ("Mk" <> nm)
  instNm = nm <> (intercalate "_" $ toList =<< tys)
  memTys = map typeOf members
  tyConTy = foldl tFn (mkDictType nm tys) memTys
  applied = dictTy :< Apply tyCon (map (\(_ :< Value _ [([], e)]) -> e) members)
  dictTy = mkAnn $ mkDictType nm tys
  valTy  = mkAnn $ constrain supers (mkDictType nm tys)
  in (valTy) :< Value instNm [([], applied)]
  where
  mkDictType nm args = foldl TAp (TConstructor nm) args

dataFromDecl :: [Constraint Name] -> Name -> [Name] -> [Decl TypedAnn] -> [Decl TypedAnn]
dataFromDecl superTraits name vars members = let
  dataRec = (:<) (TyAnn Nothing (Kind dataKind)) . Data name vars . pure . foldl TAp (TConstructor name)
  dataKind = foldl (\c _ -> c `KFn` Star) Star vars
  tyName = "Mk" <> name
  memTys = map typeOf members
  recTy  = foldl (\l r -> l `TAp` (TVar r)) (TConstructor name) vars
  memNms = map sigNm members
  superTys = []
  accessors = zipWith (\nm ix -> mkAccessor tyName recTy (memTys ++ superTys) nm ix) memNms [1..length members]
  -- figure out the kind of new datatype
  in dataRec (superTys ++ memTys) : accessors
  where
  sigNm (_ :< Signature nm _) = nm

mkAccessor tyNm recTy tys fldNm ix = let
  val = mkAnn valTy :< Var "el"
  valTy = tys !! (ix - 1)
  accessorTy = recTy `tFn` valTy
  in mkAnn valTy :< Value fldNm [([mkPattern recTy tyNm tys ix], val)]

mkPattern :: Type Name -> Name -> [Type Name] -> Int -> Pat TypedAnn
mkPattern recTy recNm tys ix = let
  prePats  = replicate (ix - 1) Wildcard
  postPats = replicate (length tys - ix) Wildcard
  focused  = pure $ PVar "el"
  annPats   = zipWith (:<) (map (SynAnn) tys) $ prePats <> focused <> postPats
  in mkAnn recTy :< Destructor recNm annPats

mkAnn = SynAnn
{-

Given MkShow (A a) b c

fn ShowA (MkShow a _ _)
fn b (MkShow _ b _)
-}


valueFromInst :: [Constraint Name] -> Name -> [Type Name] -> [Decl TypedAnn] -> Decl TypedAnn
valueFromInst = error "todo"

