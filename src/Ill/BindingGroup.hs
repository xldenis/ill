module Ill.BindingGroup where

import           Ill.Prelude

import           Ill.Syntax.Expression
import           Ill.Syntax.Pattern

import           Ill.Syntax
import Ill.Error

import           Control.Monad.Error.Class

import           Data.Set                  (Set, insert, notMember, singleton)
import           Data.Graph
import           Data.List                 (intersect, groupBy, sortOn)

type Ident = String

data BindingGroup a
  = ValueBG  [Decl a]
  | DataBG   [Decl a]
  | OtherBG  (Decl a)
  deriving (Show, Eq)

data BoundModules a = BoundModules
  { classDecls :: [BindingGroup a]
  , instDecls  :: [BindingGroup a]
  , valueDecls :: [BindingGroup a]
  , otherDecls :: [BindingGroup a]
  , dataDecls  :: [BindingGroup a]
  } deriving (Show, Eq)

bgNames (ValueBG ds) = map valueName ds
  where
  valueName (_ :< Value n _) = n
  valueName _ = error "ValueBG has non Value element"
bgNames (DataBG  ds) = map dataName ds
  where
  dataName (_ :< Data n _ _) = n
  dataName _ = error "DataBG has non Data element"

bgNames (OtherBG d) = []

fromBindingGroups :: [BindingGroup a] -> [Decl a]
fromBindingGroups bgs = fromBG =<< bgs
  where
  fromBG (ValueBG ds) = ds
  fromBG (DataBG  ds) = ds
  fromBG (OtherBG d) = pure d

bindingGroups :: MonadError MultiError m => [Decl a] -> m (BoundModules a)
bindingGroups ds = do
  let dataDecls = filter isDataDecl ds
      valueDecls = filter isValue ds ++ filter isSignature ds
      dataBGs = dataBindingGroups dataDecls
      valueBGs = valueBindingGroups valueDecls
      otherCond  = not <$> foldr1 (liftM2 (||)) [isValue, isDataDecl, isSignature, isImpl, isDecl]
      others = map OtherBG $ filter otherCond ds
      traitName (OtherBG (_  :< TraitImpl _ n _ _)) = n

  instBGs <- sortedInstances (filter isDecl ds) (filter isImpl ds)

  return $ BoundModules
    { dataDecls = dataBGs
    , instDecls = instBGs
    , classDecls = map OtherBG (filter isDecl ds)
    , valueDecls = valueBGs
    , otherDecls = others
    }

sccToDecl :: SCC (Decl a) -> [Decl a]
sccToDecl (AcyclicSCC d)  = [d]
sccToDecl (CyclicSCC [d]) = [d]
sccToDecl (CyclicSCC ds)  = ds

sortedInstances :: MonadError MultiError m => [Decl a] -> [Decl a] -> m [BindingGroup a]
sortedInstances decls impls = let
  groupedByTrait = map (\x -> (traitName $ head x, x)) $ groupBy (\x y -> traitName x == traitName y)  (sortOn traitName impls)

  graphList = map (\(name, is) -> (is, name, concat . maybeToList $ name `lookup` superTraitDict)) groupedByTrait
  traitName   (_ :< TraitImpl _ n _ _) = n
  superTraits (_ :< TraitImpl c _ _ _) = map fst c
  superTraitDict = map traitDictToTuple decls
  in concat <$> mapM checkDag (stronglyConnComp graphList)
  where
  traitDictToTuple (_ :< TraitDecl c n _ _) = (n, map fst c)
  traitDictToTuple _ = error "non trait decl was found when sorting traits"
  checkDag (AcyclicSCC d)  = return $ map OtherBG d
  checkDag (CyclicSCC [d]) = return $ map OtherBG d
  checkDag _ = throwError $ InternalError "cycle in traits"
-- Check for type synonym cycles in SCC
dataBindingGroups :: [Decl a] -> [BindingGroup a]
dataBindingGroups ds = let
  dataDecls = filter isDataDecl ds
  allDataDecls = map dataName dataDecls
  graphList = map graphNode dataDecls
  graphNode v = (v, dataName v, dataUsedNames v `intersect` allDataDecls)
  in map (DataBG . sccToDecl) (stronglyConnComp graphList)
  where dataName (_ :< Data n _ _) = n

dataUsedNames :: Decl a -> [Ident]
dataUsedNames (_ :< Data n _ cons) = n : (cons >>= typeUsedName)

typeUsedName :: Type Ident -> [Ident]
typeUsedName (TVar _)         = []
typeUsedName (TAp l r)        = typeUsedName l ++ typeUsedName r
typeUsedName (TConstructor t) = [t]
typeUsedName (Arrow l r)      = typeUsedName l ++ typeUsedName r
typeUsedName _                = [] -- incorrect handling of constaints for now

-- todo intersect w names from module
valueBindingGroups :: [Decl a] -> [BindingGroup a]
valueBindingGroups values = let
  -- values = filter isValue ds
  graphList = map graphNode values
  graphNode v = (v, valueName v, (signatureName $ valueName v) : valueUsedName v `intersect` allValues)
  allValues = map valueName values
  in map (ValueBG . sccToDecl) (stronglyConnComp graphList)
  where valueName (_ :< Value n _) = n
        valueName (_ :< Signature n _) = signatureName n
        valueBody (Value _ v) = v
        signatureName n = n ++ "_sig"

valueUsedName :: Decl a -> [Ident]
valueUsedName (_ :< Value n alts) = let
  s = singleton n
  f pats ex' = let
    s' = foldl (\acc p -> fst $ patUsedName acc p) s pats
    in getUsedNames s' ex'
  in concatMap (uncurry f) alts
valueUsedName (_ :< Signature n _) = [n]

expUsedName :: Set Ident -> Expr a -> (Set Ident, [Ident])
expUsedName s (_ :< Apply a bs) = let
  aNms = getUsedNames s a
  bNms = concatMap (getUsedNames s) bs
  in (s, aNms ++ bNms)
expUsedName s (_ :< BinOp op l r) =
  let (_, lms) = expUsedName s l
      (_, rms) = expUsedName s r
      (_, oms) = expUsedName s op
  in (s, oms ++ lms ++ rms)
expUsedName s (_ :< Assign names vals) =
  let s' = foldl (flip insert) s names
      vNms = map (snd . expUsedName s') vals
  in (s', concat vNms)
expUsedName s (_ :< Case e branches) = -- need to bind branches
  let eNms = getUsedNames s e
      bNms = map (\(pat, value ) ->
        let s' = fst $ patUsedName s pat
        in snd $ expUsedName s' value
        ) branches
  in (s, eNms ++ concat bNms)
expUsedName s (_ :< If e l r) =
  let eNms = getUsedNames s e
      lNms = getUsedNames s l
      rNms = getUsedNames s r
  in (s, eNms ++ lNms ++ rNms)
expUsedName s (_ :< Lambda ps body) =
  let s' = foldl (\acc n -> fst $ patUsedName acc n) s ps
  in expUsedName s' body
expUsedName s (_ :< Var n) | n `notMember` s = (s, [n])
expUsedName s (_ :< Body es) =
  let (_, bNms) = foldl (\(s', nms) n -> (nms ++) <$> expUsedName s' n) (s, []) es
  in (s, bNms)
expUsedName s _ = (s, [])

getUsedNames :: Set Ident -> Expr a -> [Ident]
getUsedNames s e = snd $ expUsedName s e

patUsedName :: Set Ident -> Pat a -> (Set Ident, [Ident])
patUsedName s (_ :< Destructor _ pats) = (foldl (\s' p -> fst $ patUsedName s' p) s pats, [])
patUsedName s (_ :< PVar nm) = (nm `insert` s, [])
patUsedName s (_ :< PLit _) = (s, [])
patUsedName s (_ :< Wildcard) = (s, [])
