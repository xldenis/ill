module Ill.Desugar where

import           Ill.Syntax.Expression
import           Ill.Syntax.Pattern

import           Ill.Syntax

import           Control.Comonad.Cofree
import           Data.Set                  (Set (..), insert, notMember, singleton)

import           Data.Graph
import           Data.List                 (intersect)

import           Control.Monad.Error.Class

type Ident = String

data ValueBindGroup a = ValBind (SCC (Decl a))
data DataBindGroup  a = DataBind (SCC (Decl a))

dataBindingGroup :: [Decl a] -> [DataBindGroup a]
dataBindingGroup ds = let
  dataDecls = filter isDataDecl ds
  allDataDecls = map dataName dataDecls
  graphList = map graphNode dataDecls
  graphNode v@(_ :< val) = (v, dataName v, dataUsedNames v `intersect` allDataDecls)
  in map DataBind (stronglyConnComp graphList)
  where dataName (_ :< Data n _) = n

bindingToDecl :: MonadError String m => ValueBindGroup a -> m [Decl a]
bindingToDecl (ValBind (AcyclicSCC d)) = return [d]
bindingToDecl (ValBind (CyclicSCC ds)) = throwError "WTF?"


dataUsedNames :: Decl a -> [Ident]
dataUsedNames (_ :< Data n cons) = n : (cons >>= typeUsedName)

typeUsedName :: Type Ident -> [Ident]
typeUsedName (TVar _)        = []
typeUsedName (TAp l r)       = typeUsedName l ++ typeUsedName r
typeUsedName (Constructor t) = [t]
typeUsedName (Arrow l r)     = typeUsedName l ++ typeUsedName r
typeUsedName _               = [] -- incorrect handling of constaints for now

-- todo intersect w names from module
valueBindingGroups :: [Decl a] -> [ValueBindGroup a]
valueBindingGroups ds = let
  values = filter isValue ds
  graphList = map (graphNode) values
  graphNode v@(_ :< val) = (v, valueName v, valueUsedName v)
  allValues = map valueName values
  in map (ValBind) (stronglyConnComp graphList)
  where valueName (_ :< Value n _) = n
        valueBody (Value _ v) = v

valueUsedName :: Decl a -> [Ident]
valueUsedName (_ :< Value n alts) = let
  s = singleton n
  f pats exp = let
    s' = foldl (\(s') n -> fst $ patUsedName s' n) s pats
    in getUsedNames s' exp
  in concat $ map (\(p, e) -> f p e) alts

expUsedName :: Set Ident -> Expr a -> (Set Ident, [Ident])
expUsedName s (_ :< Apply a bs) = let
  aNms = getUsedNames s a
  bNms = concat $ map (getUsedNames s) bs
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
        let (s') = foldl (\(s') n -> fst $ patUsedName s' n) s pats
        in snd $ expUsedName s' value
        ) branches
  in (s, eNms ++ concat bNms)
expUsedName s (_ :< If e l r) =
  let eNms = getUsedNames s e
      lNms = getUsedNames s l
      rNms = getUsedNames s r
  in (s, eNms ++ lNms ++ rNms)
expUsedName s (_ :< Lambda ps body) =
  let (s') = foldl (\(s') n -> fst $ patUsedName s' n) s ps
  in expUsedName s' body
expUsedName s (_ :< Var n) | n `notMember` s = (s, [n])
expUsedName s (_ :< Body es) =
  let (_, bNms) = foldl (\(s', nms) n -> fmap (nms ++) $ expUsedName s' n) (s, []) es
  in (s, bNms)
expUsedName s e = (s, [])

getUsedNames :: Set Ident -> Expr a -> [Ident]
getUsedNames s e = snd $ expUsedName s e

patUsedName :: Set Ident -> Pattern -> (Set Ident, [Ident])
patUsedName s (Destructor _ pats) = (foldl (\s' p -> fst $ patUsedName s' p) s pats, [])
patUsedName s (PVar nm) = (nm `insert` s, [])
patUsedName s (Wildcard) = (s, [])
