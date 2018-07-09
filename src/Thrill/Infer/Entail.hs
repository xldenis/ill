module Thrill.Infer.Entail where

import           Thrill.Prelude

import           Control.Monad.State

import qualified Data.Map as M (lookup)

import           Thrill.Syntax          (QualifiedName)
import           Thrill.Syntax.Type

import           Thrill.Infer.Monad as M
import           Thrill.Infer.Types

import           Thrill.Error

import Thrill.Syntax.Pretty
{-

-}

entails :: TraitDict -> InstanceDict -> [Constraint QualifiedName] -> Constraint QualifiedName -> Bool
entails td id preds constraint = go constraint

  where

  go :: Constraint QualifiedName -> Bool
  go cons = do
    let superTraits = map (goalsBySuperTrait td) preds
        instances   = goalsByInst id cons
    any (cons `elem`) superTraits || case instances of
      Nothing       -> False
      Just subcons' -> all go subcons'
  constraintName (n, _) = n

goalsBySuperTrait :: TraitDict -> Constraint QualifiedName -> [Constraint QualifiedName]
goalsBySuperTrait dict trait@(n, _) = trait : (superTraitDecl >>= superTraits >>= superTraitsFor)
  where
  superTraitDecl = lookup n dict & maybeToList
  superTraitsFor c@(n, _) = goalsBySuperTrait dict c

goalsByInst :: InstanceDict -> Constraint QualifiedName -> Maybe [Constraint QualifiedName]
goalsByInst dict cons = snd <$> matchInst dict cons

matchInst :: InstanceDict -> Constraint QualifiedName -> Maybe (InstanceEntry, [Constraint QualifiedName])
matchInst dict (trait, ty) =
  asum $ map tryInst' $ M.lookup trait dict & concat

  where

  tryInst' :: InstanceEntry -> Maybe (InstanceEntry, [Constraint QualifiedName])
  tryInst' i@(InstanceEntry instTy cons _) = do
    let instHead = TAp (TConstructor trait) instTy
        searchHead = TAp (TConstructor trait) ty

    subs <- subsume instHead searchHead

    return (i, map (substituteConstraint subs) cons)
    where

    substituteConstraint :: [(QualifiedName, Type QualifiedName)] -> Constraint QualifiedName -> Constraint QualifiedName
    substituteConstraint sub (cons, ty) = (cons, replaceTypeVars sub ty)

inHnf :: Constraint QualifiedName -> Bool
inHnf (n, t) = hnf t
  where
  hnf (TVar t)         = True
  hnf (TConstructor _) = False
  hnf (TAp t _)        = hnf t
  hnf (Arrow l r)      = hnf l
  hnf _                = True

toHnf :: InstanceDict -> Constraint QualifiedName -> Check [Constraint QualifiedName]
toHnf id p | inHnf p   = return [p]
           | otherwise = case goalsByInst id p of
                          Just ps -> toHnfs id ps
                          Nothing -> throwError $ MissingTraitImpl [p]

toHnfs :: InstanceDict -> [Constraint QualifiedName] -> Check [Constraint QualifiedName]
toHnfs id ps = concat <$> mapM (toHnf id) ps

simplify   :: TraitDict -> InstanceDict -> [Constraint QualifiedName] -> [Constraint QualifiedName]
simplify td id = loop []
 where loop rs []                               = rs
       loop rs (p:ps) | entails td id (rs++ps) p = loop rs ps
                      | otherwise               = loop (p:rs) ps

reduce :: [Constraint QualifiedName] -> Check [Constraint QualifiedName]
reduce constraints = do
  env <- getEnv
  hnfed <- toHnfs (traitDictionaries env) constraints

  return $ simplify (traits env) (traitDictionaries env) hnfed


checkSufficientConstraints :: [Constraint QualifiedName] -> [Constraint QualifiedName] -> Check () -- maybe [Constraint QualifiedName] ??
checkSufficientConstraints assumed inferred = do
  inf' <- reduce inferred
  env <- getEnv

  let bad = filter (not . entails (traits env) (traitDictionaries env) assumed) inf'

  when (not (null bad)) $ throwError $ InsufficientConstraints bad
