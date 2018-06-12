module Ill.BindingGroup where

import           Ill.Prelude

import           Ill.Syntax.Expression
import           Ill.Syntax.Pattern

import           Ill.Syntax
import           Ill.Error as Ill

import           Control.Monad.Error.Class

import           Data.Set                  (Set, insert, notMember, singleton)
import           Data.Graph
import           Data.List                 (intersect, groupBy, sortOn)

type ModuleBG nm a = Module' (BoundModules nm a)

data BindingGroup nm a
  = ValueBG  [Decl nm a]
  | DataBG   [Decl nm a]
  | OtherBG  (Decl nm a)
  deriving (Show, Eq)

data BoundModules nm a = BoundModules
  { classDecls :: [BindingGroup nm a]
  , instDecls  :: [BindingGroup nm a]
  , valueDecls :: [BindingGroup nm a]
  , otherDecls :: [BindingGroup nm a]
  , dataDecls  :: [BindingGroup nm a]
  } deriving (Show, Eq)

instance Semigroup (BoundModules nm a) where
  mod1 <> mod2 = BoundModules
    { classDecls = classDecls mod1 <> classDecls mod2
    , instDecls = instDecls mod1 <> instDecls mod2
    , valueDecls = valueDecls mod1 <> valueDecls mod2
    , otherDecls = otherDecls mod1 <> otherDecls mod2
    , dataDecls = dataDecls mod1 <> dataDecls mod2
    }

bgNames (ValueBG ds) = map valueName ds
  where
  valueName (_ :< Value n _) = n
  valueName _ = error "ValueBG has non Value element"
bgNames (DataBG  ds) = map dataName ds
  where
  dataName (_ :< Data n _ _) = n
  dataName _ = error "DataBG has non Data element"

bgNames (OtherBG d) = []

fromBindingGroups :: [BindingGroup nm a] -> [Decl nm a]
fromBindingGroups bgs = fromBG =<< bgs
  where
  fromBG (ValueBG ds) = ds
  fromBG (DataBG  ds) = ds
  fromBG (OtherBG d) = pure d

bindingGroups :: (MonadError (Ill.Error b) m) => Module Name a -> m (Module' (BoundModules Name a))
bindingGroups (Module nm ds) = do
  let dataDecls = filter isDataDecl ds
      valueDecls = filter isValue ds ++ filter isSignature ds
      dataBGs = dataBindingGroups dataDecls
      valueBGs = valueBindingGroups valueDecls
      otherCond  = not <$> foldr1 (liftM2 (||)) [isValue, isDataDecl, isSignature, isImpl, isDecl]
      others = map OtherBG $ filter otherCond ds
      traitName (OtherBG (_  :< TraitImpl _ n _ _)) = n

  instBGs <- sortedInstances (filter isDecl ds) (filter isImpl ds)
  declBGs <- sortedClassDeclarations (filter isDecl ds)
  return . Module nm $ BoundModules
    { dataDecls = dataBGs
    , instDecls = instBGs
    , classDecls = declBGs
    , valueDecls = valueBGs
    , otherDecls = others
    }

sccToDecl :: SCC (Decl Name a) -> [Decl Name a]
sccToDecl (AcyclicSCC d)  = [d]
sccToDecl (CyclicSCC [d]) = [d]
sccToDecl (CyclicSCC ds)  = ds

sortedClassDeclarations :: (MonadError (Ill.Error b) m) => [Decl Name a] -> m [BindingGroup Name a]
sortedClassDeclarations decls = let
  graphNode v = (v, traitName $ unwrap v, supers v)
  graphList = map graphNode decls
  supers (_ :< d@TraitDecl{}) = map fst (traitSuperclasses d)
  in mapM checkDag (stronglyConnComp graphList)
  where
  checkDag (AcyclicSCC d)  = return $ OtherBG d
  checkDag (CyclicSCC [d]) = return $ OtherBG d

sortedInstances :: (MonadError (Ill.Error b) m) => [Decl Name a] -> [Decl Name a] -> m [BindingGroup Name a]
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
  checkDag _ = throwError $ Error
    { errKind = "binding-group"
    , errHeader = pretty "Cycle in trait implementations"
    , errSummary = pretty "put traits that actually form the cycle here!"
    , errHints = []
    }

-- Check for type synonym cycles in SCC
dataBindingGroups :: [Decl Name a] -> [BindingGroup Name a]
dataBindingGroups ds = let
  dataDecls = filter isDataDecl ds
  allDataDecls = map dataName dataDecls
  graphList = map graphNode dataDecls
  graphNode v = (v, dataName v, dataUsedNames v `intersect` allDataDecls)
  in map (DataBG . sccToDecl) (stronglyConnComp graphList)
  where dataName (_ :< Data n _ _) = n

dataUsedNames :: Decl Name a -> [Name]
dataUsedNames (_ :< Data n _ cons) = n : (cons >>= typeUsedName)

typeUsedName :: Type nm -> [nm]
typeUsedName (TVar _)         = []
typeUsedName (TAp l r)        = typeUsedName l ++ typeUsedName r
typeUsedName (TConstructor t) = [t]
typeUsedName (Arrow l r)      = typeUsedName l ++ typeUsedName r
typeUsedName _                = [] -- incorrect handling of constaints for now

-- todo intersect w names from module
valueBindingGroups :: [Decl Name a] -> [BindingGroup Name a]
valueBindingGroups values = let
  -- values = filter isValue ds
  graphList = map graphNode values
  graphNode v = (v, valueName v, (signatureName $ valueName v) : valueUsedName v `intersect` allValues)
  allValues = map valueName values
  in map (ValueBG . sccToDecl) (stronglyConnComp graphList)
  where
  valueName :: Decl Name a -> Name
  valueName (_ :< Value n _) = n
  valueName (_ :< Signature n _) = signatureName n
  valueBody (Value _ v) = v

  signatureName n = (++ "_sig") n

valueUsedName :: Decl Name a -> [Name]
valueUsedName (_ :< Value n alts) = let
  s = singleton n
  f pats ex' = let
    s' = foldl (\acc p -> fst $ patUsedName acc p) s pats
    in getUsedNames s' ex'
  in concatMap (uncurry f) alts
valueUsedName (_ :< Signature n _) = [n]

expUsedName :: Set Name -> Expr' Name a -> (Set Name, [Name])
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

getUsedNames :: Set Name -> Expr' Name a -> [Name]
getUsedNames s e = snd $ expUsedName s e

patUsedName :: Set Name -> Pat' Name a -> (Set Name, [Name])
patUsedName s (_ :< Destructor _ pats) = (foldl (\s' p -> fst $ patUsedName s' p) s pats, [])
patUsedName s (_ :< PVar nm) = (nm `insert` s, [])
patUsedName s (_ :< PLit _) = (s, [])
patUsedName s (_ :< Wildcard) = (s, [])
