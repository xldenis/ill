module Ill.Syntax.Core where


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

import           Ill.Infer.Monad (TypeAnn (..), TypedAnn (..))

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

{-
  1. fix binding of new vars
  2. Add lambda abstraction around match
  3.
-}

type MatchResult a = Expr a -> Expr a

declToEqns :: Decl a -> [Eqn a]
declToEqns (_ :< Value _ eqns) = eqns
declToEqns _                   = []

-- actually bind the binders
simplifyPatterns :: Decl a -> Decl a
simplifyPatterns (a :< Value n eqns) = let
  binders = enumFromTo 0 (length . fst $ head eqns) & map (\i -> "x" ++ show i)
  matchResult = match binders eqns
  failure = (undefined :< Var "failedPattern")
  exp = matchResult failure
  eqn' = [([], exp)]
  in a :< Value n eqn'

match :: [String] -> [Eqn a] -> (MatchResult a)
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
    WildG  -> error "wildcard patterns are not implemented"
    LitG   -> error "literal patterns are not implemented"

  matchVarPat :: [String] -> [Eqn a] -> MatchResult a
  matchVarPat (_:vars) = match vars . shiftEqnPats

  -- handle failure
  -- figure out way to build proper annotations
  -- get the types of patterns
  matchConPat :: [String] -> [[Eqn a]] -> MatchResult a
  matchConPat (var : vars) groups = let
    branches = map (matchOneConsPat vars) groups

    in \fail -> undefined :< Case (undefined :< Var var) (map (updateEqn fail) branches)
    where


    mkTypedAnn ty = Ann undefined (Type ty)
    updateEqn f (pats, mr) = (pats, mr f)

  matchOneConsPat :: [String] -> [Eqn a] -> (Pat a, MatchResult a)
  matchOneConsPat vars group = let
    eqns' = map shiftCons group
    (a :< Destructor c args1) =  head . fst $ head group
    arg_vars = makeVarNames args1
    arg_tys  = map extract args1
    rhs = match (arg_vars ++ vars) eqns'

    in (a :< Destructor c (zipWith (\t v -> t :< PVar v) arg_tys arg_vars), rhs)

  shiftCons ((_ :< Destructor _ ps) : xs, rhs) = (ps ++ xs, rhs)

  shiftEqnPats = map (\(pats, eq) -> (tail pats, eq))


makeVarNames :: [Pat a] -> [String]
makeVarNames ((_ :< PVar n) : ps) = n : makeVarNames ps
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
