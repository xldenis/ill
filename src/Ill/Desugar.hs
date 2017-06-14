module Ill.Desugar where

import           Ill.Syntax.Expression
import           Ill.Syntax.Pattern

import           Ill.Syntax

import           Control.Comonad.Cofree
import           Data.Set                  (Set, insert, notMember, singleton)

import           Data.Graph
import           Data.List                 (intersect)

import           Control.Monad.Error.Class

type Ident = String

data BindingGroup a
  = ValueBG  [Decl a]
  | DataBG   [Decl a]
  | OtherBG  (Decl a)
  deriving (Show, Eq)

bgNames (ValueBG ds) = map (\(_ :< (Value n _)) -> n) ds
bgNames (DataBG  ds) = map (\(_ :< (Data n _)) -> n) ds
bgNames (OtherBG d) = []

bindingGroups :: [Decl a] -> [BindingGroup a]
bindingGroups ds = let
  dataDecls = filter isDataDecl ds
  valueDecls = filter isValue ds
  dataBGs = dataBindingGroups dataDecls
  valueBGs = valueBindingGroups valueDecls

  others = map OtherBG $ filter (\d -> not (isValue d) && not (isDataDecl d)) ds
  in dataBGs ++ valueBGs ++ others

sccToDecl :: SCC (Decl a) -> [Decl a]
sccToDecl (AcyclicSCC d)  = [d]
sccToDecl (CyclicSCC [d]) = [d]
sccToDecl (CyclicSCC ds)  = ds

-- Check for type synonym cycles in SCC
dataBindingGroups :: [Decl a] -> [BindingGroup a]
dataBindingGroups ds = let
  dataDecls = filter isDataDecl ds
  allDataDecls = map dataName dataDecls
  graphList = map graphNode dataDecls
  graphNode v = (v, dataName v, dataUsedNames v `intersect` allDataDecls)
  in map (DataBG . sccToDecl) (stronglyConnComp graphList)
  where dataName (_ :< Data n _) = n

dataUsedNames :: Decl a -> [Ident]
dataUsedNames (_ :< Data n cons) = n : (cons >>= typeUsedName)

typeUsedName :: Type Ident -> [Ident]
typeUsedName (TVar _)        = []
typeUsedName (TAp l r)       = typeUsedName l ++ typeUsedName r
typeUsedName (Constructor t) = [t]
typeUsedName (Arrow l r)     = typeUsedName l ++ typeUsedName r
typeUsedName _               = [] -- incorrect handling of constaints for now

-- todo intersect w names from module
valueBindingGroups :: [Decl a] -> [BindingGroup a]
valueBindingGroups ds = let
  values = filter isValue ds
  graphList = map graphNode values
  graphNode v = (v, valueName v, valueUsedName v)
  allValues = map valueName values
  in map (ValueBG . sccToDecl) (stronglyConnComp graphList)
  where valueName (_ :< Value n _) = n
        valueBody (Value _ v) = v

valueUsedName :: Decl a -> [Ident]
valueUsedName (_ :< Value n alts) = let
  s = singleton n
  f pats ex' = let
    s' = foldl (\acc p -> fst $ patUsedName acc p) s pats
    in getUsedNames s' ex'
  in concatMap (uncurry f) alts

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
      bNms = map (\(pats, value ) ->
        let s' = foldl (\acc n -> fst $ patUsedName acc n) s pats
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

patUsedName :: Set Ident -> Pattern -> (Set Ident, [Ident])
patUsedName s (Destructor _ pats) = (foldl (\s' p -> fst $ patUsedName s' p) s pats, [])
patUsedName s (PVar nm) = (nm `insert` s, [])
patUsedName s Wildcard = (s, [])
