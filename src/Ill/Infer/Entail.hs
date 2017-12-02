module Ill.Infer.Entail where
import           Control.Monad.State
import           Control.Monad.Unify

import           Data.Foldable
import           Data.Function
import           Data.List
import           Data.Maybe
import qualified Data.Map as M (lookup)

import           Ill.Syntax          (Name)
import           Ill.Syntax.Type

import           Ill.Infer.Monad as M
import           Ill.Infer.Types

import           Ill.Error

{-

-}

entails :: TraitDict -> InstanceDict -> [Constraint Name] -> Constraint Name -> Bool
entails td id preds constraint = go constraint

  where

  go :: Constraint Name -> Bool
  go cons = do
    let superTraits = map (goalsBySuperTrait td) preds
        instances   = goalsByInst id cons
    any (cons `elem`) superTraits || case instances of
      Nothing       -> False
      Just subcons' -> all go subcons'
  constraintName (n, _) = n

goalsBySuperTrait :: TraitDict -> Constraint Name -> [Constraint Name]
goalsBySuperTrait dict trait@(n, _) = trait : (superTraitDecl >>= superTraits >>= superTraitsFor)
  where
  superTraitDecl = lookup n dict & maybeToList
  superTraitsFor c@(n, _) = goalsBySuperTrait dict c

goalsByInst :: InstanceDict -> Constraint Name -> Maybe [Constraint Name]
goalsByInst dict cons = snd <$> matchInst dict cons

matchInst :: InstanceDict -> Constraint Name -> Maybe (InstanceEntry, [Constraint Name])
matchInst dict (trait, tys) =
  asum $ map tryInst' $ M.lookup trait dict & concat

  where

  tryInst' :: InstanceEntry -> Maybe (InstanceEntry, [Constraint Name])
  tryInst' i@(InstanceEntry instTys cons _) = do
    let instHead = foldl TAp (TConstructor trait) instTys
        searchHead = foldl TAp (TConstructor trait) tys

    subs <- subsume instHead searchHead

    return (i, map (substituteConstraint subs) cons)
    where

    substituteConstraint :: [(Name, Type Name)] -> Constraint Name -> Constraint Name
    substituteConstraint sub (cons, tys) = (cons, map (replaceTypeVars sub) tys)

inHnf :: Constraint Name -> Bool
inHnf (n, [t]) = hnf t
  where
  hnf (TVar t)         = True
  hnf (TConstructor _) = False
  hnf (TAp t _)        = hnf t
  hnf (Arrow l r)      = hnf l
  hnf _                = True

toHnf :: InstanceDict -> Constraint Name -> Check [Constraint Name]
toHnf id p | inHnf p   = return [p]
           | otherwise = case goalsByInst id p of
                          Just ps -> toHnfs id ps
                          Nothing -> throwError $ MissingTraitImpl [p]

toHnfs :: InstanceDict -> [Constraint Name] -> Check [Constraint Name]
toHnfs id ps = concat <$> mapM (toHnf id) ps

simplify   :: TraitDict -> InstanceDict -> [Constraint Name] -> [Constraint Name]
simplify td id = loop []
 where loop rs []                               = rs
       loop rs (p:ps) | entails td id (rs++ps) p = loop rs ps
                      | otherwise               = loop (p:rs) ps

reduce :: [Constraint Name] -> Check [Constraint Name]
reduce constraints = do
  env <- getEnv
  hnfed <- toHnfs (traitDictionaries env) constraints

  return $ simplify (traits env) (traitDictionaries env) hnfed


checkSufficientConstraints :: [Constraint Name] -> [Constraint Name] -> Check () -- maybe [Constraint Name] ??
checkSufficientConstraints assumed inferred = do
  inf' <- reduce inferred
  env <- getEnv
  let bad = filter (not . entails (traits env) (traitDictionaries env) assumed) inf'

  when (not (null bad)) $ internalError (show bad)
