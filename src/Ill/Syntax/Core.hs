module Ill.Syntax.Core where


 -- AST -> Core (Desugaring)

{-
  Passes:

  1. Push in cases
  2. Desugar to core
  3. Type class application
  4. ?????
-}
import Ill.Syntax

import Data.List
import qualified Data.Map.Strict as Map

data PatGroup = VarG | ConG String | WildG | LitG
  deriving (Show, Eq)

patGroup (Destructor c _) = ConG c
patGroup (PVar _) = VarG
patGroup (Wildcard) = WildG
patGroup (PLit _) = LitG

type Eqn a = ([Pattern], a)

{-
  1. fix binding of new vars
-}

match :: [String] -> Type Name -> [Eqn (Expr a)] -> (Expr a)
match vars ty = undefined
  where

  matchGroup ty eqns@((group, _) : _) = case group of
    VarG  -> matchVarPat vars ty (map snd eqns)
    ConG _ -> matchConPat vars ty (subGroup [(c, e) | (ConG c, e) <- eqns])
    WildG -> undefined
    LitG  ->  undefined

  matchVarPat :: [String] -> Type String -> [Eqn (Expr a)] -> Expr a
  matchVarPat vars ty = match vars ty . shiftEqnPats

  matchConPat :: [String] -> Type String -> [[Eqn (Expr a)]] -> Expr a
  matchConPat (var : vars) ty groups = let
    branches = map (matchOneConsPat vars ty) groups

    in undefined :< Case (undefined :< Var var) branches

  matchOneConsPat :: [String] -> Type String -> [Eqn (Expr a)] -> (Pattern, Expr a)
  matchOneConsPat vars ty group = let
    eqns' = map shiftCons group
    (Destructor c args1) =  head . fst $ head group
    arg_vars = makeVarNames args1
    rhs = match (vars ++ arg_vars) ty eqns'

    in (Destructor c (map PVar arg_vars), rhs)

  shiftCons ((Destructor _ ps : xs), rhs) = (ps ++ xs, rhs)

  shiftEqnPats = map (\(pats, eq) -> (tail pats, eq))


makeVarNames :: [Pattern] -> [String]
makeVarNames (PVar n : ps) = n : makeVarNames ps
makeVarNames [] = []

groupPatterns :: [Eqn a] -> [[(PatGroup, Eqn a)]]
groupPatterns alts = groupBy sameGroup (map (\p -> (patGroup (firstPat p), p)) alts)
  where

  firstPat (pats, e) = head pats

  sameGroup a b = fst a == fst b

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