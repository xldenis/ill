module Ill.Desugar.Cases where


 -- AST -> Core (Desugaring)

{-
  Passes:

  1. Push in cases
  2. Desugar to core
  3. Type class application
  4. ?????
-}
import           Ill.Syntax

import           Data.Function
import           Data.List
import qualified Data.Map.Strict as Map

import Control.Comonad
import Control.Comonad.Cofree

data PatGroup = VarG | ConG String | WildG | LitG
  deriving (Show, Eq)

patGroup :: Pattern a -> PatGroup
patGroup (Destructor c _) = ConG c
patGroup (PVar _)         = VarG
patGroup (Wildcard)       = WildG
patGroup (PLit _)         = LitG

type Eqn a = ([Pat a], Expr a)

eqnsPats = fst . head
eqnsExpr = snd . head

{-
  1. fix binding of new vars
  2. Add lambda abstraction around match
  3. actually fail non-total matches
-}

type MatchResult a = Expr a -> Expr a

declToEqns :: Decl TypedAnn -> [Eqn TypedAnn]
declToEqns (_ :< Value _ eqns) = eqns
declToEqns _                   = []

-- actually bind the binders
simplifyPatterns :: Decl TypedAnn -> Decl TypedAnn
simplifyPatterns v@(a :< Value n eqns) = let
  binders = enumFromTo 1 (length . fst $ head eqns) & map (\i -> "x" ++ show i)
  binderTy = init . unwrapFnType $ typeOf v
  retTy = typeOf . snd $ head eqns
  matchResult = match binders eqns
  failure = (undefined :< Var "failedPattern")
  exp = matchResult failure
  eqn' = [([], mkAbs (zipWith (\p t -> mkTypedAnn t :< PVar p) binders binderTy) retTy exp)]
  in a :< Value n eqn'
  where

  mkAbs binders retTy val = mkTypedAnn retTy :< Lambda binders val
simplifyPatterns v = v

mkTypedAnn ty = SynAnn ty

match :: [String] -> [Eqn TypedAnn] -> (MatchResult TypedAnn)
match [] eqns | all (null . fst) eqns = const . snd $ head eqns
match vars eqns = let
  grouped = groupPatterns eqns
  match_r = matchGroups grouped
  in \fail -> foldr ($) fail match_r
  where

  matchGroups eqns = map matchGroup eqns

  matchGroup eqns@((group, _) : _) = case group of
    VarG   -> matchVarPat vars (map snd eqns)
    ConG _ -> matchConPat vars (subGroup [(c, e) | (ConG c, e) <- eqns])
    WildG  -> matchWildcardPat vars (map snd eqns)
    LitG   -> error "literal patterns are not implemented"

  matchWildcardPat (_ : vars) eqns = \fail -> match vars (shiftEqnPats eqns) fail

  matchVarPat :: [String] -> [Eqn TypedAnn] -> MatchResult TypedAnn
  matchVarPat (v : vars) eqns = \fail -> let
    a :< PVar pat = head $ eqnsPats eqns
    matchResult = match vars (shiftEqnPats eqns) fail
    in case matchResult of
      _ :< Body xs -> mkBody $ if pat /= v then  mkAssign pat (a :< Var v) : xs  else xs
      y            -> mkBody $ if pat /= v then  mkAssign pat (a :< Var v) : [y] else [y]

  mkAssign :: String -> Expr TypedAnn -> Expr TypedAnn
  mkAssign n e = (extract e) :< Assign [n] [e]

  mkBody :: [Expr TypedAnn] -> Expr TypedAnn
  mkBody xs = (extract $ last xs) :< Body xs

  -- handle failure
  -- figure out way to build proper annotations
  matchConPat :: [String] -> [[Eqn TypedAnn]] -> MatchResult TypedAnn
  matchConPat (var : vars) groups = let
    branches = map (matchOneConsPat vars) groups
    retTy = typeOf . snd . head $ head groups
    scrutTy = typeOf . head . fst . head $ head groups

    in \fail -> mkTypedAnn retTy :< Case (mkTypedAnn scrutTy :< Var var) (map (updateEqn fail) branches)
    where

    updateEqn f (pats, mr) = (pats, mr f)

  matchOneConsPat :: [String] -> [Eqn TypedAnn] -> (Pat TypedAnn, MatchResult TypedAnn)
  matchOneConsPat vars group = let
    eqns' = map shiftCons group
    (a :< Destructor c args1) =  head . fst $ head group
    arg_vars = makeVarNames args1
    arg_tys  = map extract args1
    rhs = match ((map (maybe "" id) arg_vars) ++ vars) eqns'
    maybeNameToPat ty (Just nm) = ty :< PVar nm
    maybeNameToPat ty Nothing   = ty :< Wildcard
    in (a :< Destructor c (zipWith (maybeNameToPat) arg_tys arg_vars), rhs)

  shiftCons ((_ :< Destructor _ ps) : xs, rhs) = (ps ++ xs, rhs)

  shiftEqnPats = map (\(pats, eq) -> (tail pats, eq))

makeVarNames :: Show a => [Pat a] -> [Maybe String]
makeVarNames ((_ :< PVar n) : ps) = Just n : makeVarNames ps
makeVarNames (_ : ps) = Nothing : makeVarNames ps
makeVarNames []            = []

groupPatterns :: [Eqn a] -> [[(PatGroup, Eqn a)]]
groupPatterns alts = groupBy sameGroup (map (\p -> (patGroup $ unwrap (firstPat p), p)) alts)
  where

  firstPat (pats, e) = head pats

  sameGroup (ConG _, _) (ConG _, _) = True
  sameGroup a b                     = fst a == fst b

subGroup group
    = map reverse $ Map.elems $ foldl accumulate Map.empty group
  where
    accumulate pg_map (pg, eqn)
      = case Map.lookup pg pg_map of
          Just eqns -> Map.insert pg (eqn:eqns) pg_map
          Nothing   -> Map.insert pg [eqn]      pg_map

    -- pg_map :: Map a [EquationInfo]
-- data Core b
--   = Lambda b (Expr b)
--   | App (Core b) (Arg b)
--   -- | Case
--   | Var Id
--   -- | Let
--   | Type Type
--   | Lit Literal
--   deriving (Show)
