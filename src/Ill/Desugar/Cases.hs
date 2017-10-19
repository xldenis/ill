module Ill.Desugar.Cases where


 -- AST -> Core (Desugaring)

{-
  Passes:

  1. Push in cases
  2. Desugar to core
  3. Type class application
  4. ?????
-}
import Control.Lens (each, _1, (%~))
import Control.Lens.Plated
import           Ill.Syntax
import           Control.Monad.Fresh
import           Data.Function
import           Data.List
import qualified Data.Map.Strict as Map

import Control.Comonad
import Control.Comonad.Cofree
import Control.Monad.Identity
import Control.Monad.State

import Data.Maybe (catMaybes)

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

desugarPatterns = simplifyPatterns

type MF  a = FreshT Identity a

runFresh = runIdentity . flip evalStateT 0 . unFreshT

simplifyPatterns :: Decl TypedAnn -> Decl TypedAnn
simplifyPatterns v@(a :< Value n eqns) = runFresh $ do
  let
    binders = enumFromTo 1 (length . fst $ head eqns) & map (\i -> "x" ++ show i)
    binderTy = init . unwrapFnType . snd . unconstrained $ typeOf v
    retTy = typeOf . snd $ head eqns
    failure = (undefined :< Var "failedPattern")
  matchResult <- match binders eqns
  exp' <- transformM simplifyCaseExpr (matchResult failure)

  let eqn' = [([], mkAbs (zipWith (\p t -> mkTypedAnn t :< PVar p) binders binderTy) retTy exp')]
  return $ a :< Value n eqn'
simplifyPatterns v = v

mkAbs binders retTy val = mkTypedAnn retTy :< Lambda binders val

simplifyCaseExpr :: MonadFresh m => Expr TypedAnn -> m (Expr TypedAnn)
simplifyCaseExpr e@(a :< Case s alts) = do
  let
  -- binder = head . unwrapFnType (typeOf e)
    eqns = alts & each . _1 %~ pure
    failure = (undefined :< Var "failedPattern")
    caseTy = typeOf e
    scrutTy = typeOf s
  (adjustments, binder) <- binderName s
  matchResult <- match [binder] eqns
  return  $ mkBody $ catMaybes [adjustments, Just $ matchResult failure]

  where
  binderName (_ :< Var nm) = pure (Nothing, nm)
  binderName e = do
    name <- prefixedName "dsCase"
    return (Just $ SynAnn tNil :< Assign [name] [e], name)

simplifyCaseExpr v = pure v

mkTypedAnn ty = SynAnn ty

mkBody :: [Expr TypedAnn] -> Expr TypedAnn
mkBody xs = (extract $ last xs) :< Body xs

match :: MonadFresh m => [String] -> [Eqn TypedAnn] -> m (MatchResult TypedAnn)
match [] eqns | all (null . fst) eqns = pure $ const . snd $ head eqns
match vars eqns = do
  let grouped = groupPatterns eqns
  match_r <- matchGroups grouped
  return $ \fail -> foldr ($) fail match_r
  where

  matchGroups :: MonadFresh m => [[(PatGroup, Eqn TypedAnn)]] -> m [MatchResult TypedAnn]
  matchGroups eqns = mapM matchGroup eqns

  matchGroup :: MonadFresh m => [(PatGroup, Eqn TypedAnn)] -> m (MatchResult TypedAnn)
  matchGroup eqns@((group, _) : _) = case group of
    VarG   -> matchVarPat vars (map snd eqns)
    ConG _ -> matchConPat vars (subGroup [(c, e) | (ConG c, e) <- eqns])
    WildG  -> matchWildcardPat vars (map snd eqns)
    LitG   -> error "literal patterns are not implemented"

  matchWildcardPat (_ : vars) eqns = match vars (shiftEqnPats eqns)

  matchVarPat :: MonadFresh m => [String] -> [Eqn TypedAnn] -> m (MatchResult TypedAnn)
  matchVarPat (v : vars) eqns = do
    let (a, pat) = fromPVar . head $ eqnsPats eqns

    matchResult <- match vars (shiftEqnPats eqns)

    return $ \fail -> case matchResult fail of
      _ :< Body xs -> mkBody $ if pat /= v then  mkAssign pat (a :< Var v) : xs  else xs
      y            -> mkBody $ if pat /= v then  mkAssign pat (a :< Var v) : [y] else [y]

    where
    fromPVar (a :< PVar pat) = (a, pat)
    fromPVar _ = error "impossible non-var pattern in when matching variable patterns"

  mkAssign :: String -> Expr TypedAnn -> Expr TypedAnn
  mkAssign n e = (extract e) :< Assign [n] [e]

  matchConPat :: MonadFresh m => [String] -> [[Eqn TypedAnn]] -> m (MatchResult TypedAnn)
  matchConPat (var : vars) groups = do
    let
      retTy = typeOf . snd . head $ head groups
      scrutTy = typeOf . head . fst . head $ head groups
    branches <- mapM (matchOneConsPat vars) groups
    -- a failure branch should be included allowing the variable branch to be inserted!
    let failBranch = ((extract . fst $ head  branches) :< Wildcard, id)
    return $ \fail -> mkTypedAnn retTy :< Case (mkTypedAnn scrutTy :< Var var) (map (updateEqn fail) (branches ++ [failBranch]))
    where

    updateEqn f (pats, mr) = (pats, mr f)

  matchOneConsPat :: MonadFresh m => [String] -> [Eqn TypedAnn] -> m (Pat TypedAnn, MatchResult TypedAnn)
  matchOneConsPat vars group = do
    let
      eqns'         = map shiftCons group
      (a, c, args1) = fromPDest . head . fst $ head group
      arg_tys       = map extract args1
      toVarPat (ty :< Wildcard) _ = ty :< Wildcard
      toVarPat (ty :< _) nm       = ty :< PVar nm
    arg_vars <- makeVarNames args1
    rhs <- match (arg_vars ++ vars) eqns'

    return $ (a :< Destructor c (zipWith toVarPat args1 arg_vars), rhs)
    where
    fromPDest (a :< Destructor c args1) = (a, c, args1)
    fromPDest _ = error "impossible non-destructor pattern in when matching destructor patterns"

  shiftCons ((_ :< Destructor _ ps) : xs, rhs) = (ps ++ xs, rhs)

  shiftEqnPats = map (\(pats, eq) -> (tail pats, eq))

makeVarNames :: (Show a, MonadFresh m) => [Pat a] -> m [String]
makeVarNames ((_ :< PVar n) : ps) = (:) <$> pure ( n) <*> makeVarNames ps
makeVarNames (_ : ps) = (:) <$> prefixedName "omg" <*> makeVarNames ps -- generate names
makeVarNames []            = pure []

prefixedName :: MonadFresh m => String -> m String
prefixedName pre = (pre ++) . show <$> freshName

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
