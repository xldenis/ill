module Ill.Desugar.Cases where


import           Control.Comonad
import           Control.Comonad.Cofree

import           Control.Lens (each, _1, (%~))
import           Control.Lens.Plated

import           Control.Monad.Fresh
import           Control.Monad.Identity
import           Control.Monad.State

import           Data.Function
import           Data.List
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes)

import           Ill.Syntax

data PatGroup = VarG | ConG String | WildG | LitG Literal
  deriving (Show, Eq)

patGroup :: Pattern a -> PatGroup
patGroup (Destructor c _) = ConG c
patGroup (PVar _)         = VarG
patGroup (Wildcard)       = WildG
patGroup (PLit l)         = LitG l

type Eqn a = ([Pat a], Expr a)

eqnsPats :: [Eqn a] -> [Pat a]
eqnsPats = fst . head

eqnsExpr :: [Eqn a] -> Expr a
eqnsExpr = snd . head

{-
  1. fix binding of new vars
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
    LitG _ -> matchLiterals vars (subGroup $ [(l, e) | (LitG l, e) <- eqns])

  matchWildcardPat (_ : vars) eqns = match vars (shiftEqnPats eqns)

  matchVarPat :: MonadFresh m => [String] -> [Eqn TypedAnn] -> m (MatchResult TypedAnn)
  matchVarPat (v : vars) eqns = do
    let (a, _) = fromPVar . head $ eqnsPats eqns
        varNames = map (snd . fromPVar) $ headEqnPats eqns

    matchResult <- match vars (shiftEqnPats eqns)

    return $ \fail -> let
      expList = bodyToList $ matchResult fail
      bindings = filter (/= v) varNames

      in mkBody $ case bindings of
        [] -> expList
        namesToChange-> mkAdjustments (a :< Var v) namesToChange : expList

    where

    bodyToList (_ :< Body xs) = xs
    bodyToList y              = [y]

    fromPVar (a :< PVar pat) = (a, pat)
    fromPVar _ = error "impossible non-var pattern in when matching variable patterns"

  mkAssign :: String -> Expr TypedAnn -> Expr TypedAnn
  mkAssign n e = (extract e) :< Assign [n] [e]

  matchLiterals :: MonadFresh m => [String] -> [[Eqn TypedAnn]] -> m (MatchResult TypedAnn)
  matchLiterals (var : vars) subGroups = do
    let
      retTy = typeOf . snd . head $ head subGroups
      scrutTy = typeOf . head . fst . head $ head subGroups

    if (scrutTy == TConstructor "String") then
      error "currently string literal matches arent implemented"
    else do
      branches <- mapM matchLiteral subGroups
      let failBranch = ((extract . fst $ head  branches) :< Wildcard, id)

      return $ \fail -> mkTypedAnn retTy :< Case (mkTypedAnn scrutTy :< Var var) (map (fmap ($ fail)) (branches ++ [failBranch]))
    where
    firstPat (pats, e) = head pats

    matchLiteral :: MonadFresh m => [Eqn TypedAnn] -> m (Pat TypedAnn, MatchResult TypedAnn)
    matchLiteral group = do
      let pat = firstPat (head group)
      match_result <- match vars (shiftEqnPats group)
      return (pat, match_result)

  matchConPat :: MonadFresh m => [String] -> [[Eqn TypedAnn]] -> m (MatchResult TypedAnn)
  matchConPat (var : vars) groups = do
    let
      retTy = typeOf . snd . head $ head groups
      scrutTy = typeOf . head . fst . head $ head groups
    branches <- mapM (matchOneConsPat vars) groups
    -- a failure branch should be included allowing the variable branch to be inserted!
    let failBranch = ((extract . fst $ head  branches) :< Wildcard, id)
    return $ \fail -> mkTypedAnn retTy :< Case (mkTypedAnn scrutTy :< Var var) (map (fmap ($ fail)) (branches ++ [failBranch]))

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

  headEqnPats :: [Eqn a] -> [Pat a]
  headEqnPats = map (head . fst)

mkAdjustments :: Expr a -> [Name] -> (Expr a)
mkAdjustments from to = let
  to' = nub to
  from' = replicate (length to') from
  in extract from :< Assign to' from'


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
