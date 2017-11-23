{-# LANGUAGE ConstraintKinds #-}
module Ill.CoreLint
( runLinter
) where
import Ill.Syntax.Pretty
import Ill.Syntax.Core
import Ill.Syntax.Type
import Ill.Syntax.Literal

import Control.Monad.State
import Control.Monad.Error

-- fix this... somehow, primitives are leaking all over the place
import Ill.Infer.Monad (names, defaultCheckEnv, env)

import Ill.Desugar

import Data.Maybe

{-
  Core Linting

  This module implements a core-language linting pass. It performs basic sanity
  and type checking. A few of the checks:

  1. Out-of-scope names
  2. Out-of-scope type vars
  3. Typechecking all terms

  This pass is meant to be run at the end of all core transformations and during
  testing.
-}

type LintM m = (MonadState LintEnv m, MonadError String m)

data LintEnv = E
  { boundNames :: [(String, Type String)]
  , boundTyVars :: [(String)]
  } deriving (Show, Eq)

runLinter :: CoreModule -> Either String ()
runLinter = flip evalState (E (names $ env defaultCheckEnv) []) . runErrorT . lintModule
  where
  lintModule mod = bindNames (map getVar $ bindings mod) $
    bindCons (constructors mod) (mapM_ lintBind (bindings mod))

  getVar (NonRec v _) = v

  bindCons conses action = do
    let x = map (\(c, (_, ty)) -> (c, ty)) conses
    orig <- get
    modify (\s -> s { boundNames = x ++ boundNames orig })
    val <- action
    modify (\s -> s { boundNames = boundNames orig })
    return val

bindNames :: LintM m => [Var] -> m a -> m a
bindNames vars act = do
  foldr bindName act vars

bindName :: (LintM m) => Var -> m a -> m a
bindName v@(Id{}) action = do
  orig <- get
  modify (\s -> s  { boundNames = (varName v, idTy v) : boundNames orig })
  val <- action
  modify (\s -> s { boundNames = boundNames orig })
  return val
bindName v@(TyVar{}) action = do
  orig <- get
  modify (\s -> s  { boundTyVars = varName v : boundTyVars orig })
  val <- action
  modify (\s -> s { boundTyVars = boundTyVars orig })
  return val

lintBind :: (LintM m) => Bind Var -> m ()
lintBind (NonRec b exp) = void $ bindName b (lintCore exp)

lookupName var = do
  names <- gets boundNames
  case var `lookup` names of
    Just x  -> return x
    Nothing -> throwError $ "the name " ++ show var ++ " could not be found."

lookupTyVar var = do
  tyvars <- gets boundTyVars
  case var `elem` tyvars of
    True  -> return ()
    False -> throwError $ "the typevariable " ++ show var ++ " could not be found."

-- Check the types of the core expression
lintCore :: LintM m => CoreExp -> m (Type String)
lintCore l@(Lambda bind exp) = do
  bodyTy <- bindName bind (lintCore exp)
  return $ makeCorrectFunTy bind bodyTy
  where
  makeCorrectFunTy id@(Id{}) bodyTy    =  idTy bind `tFn` bodyTy
  makeCorrectFunTy tv@(TyVar{}) bodyTy =  case bodyTy of
    Forall vars ty -> Forall (varName tv : vars) ty
    ty -> Forall [varName tv] ty
  -- need to build a type lambda instead of a term lambda
lintCore (Var var) = lookupName var
lintCore ap@(App f t@(Type ty)) = do
  lintCore t
  fTy <- lintCore f

  let (tVar, fTy') = splitForall fTy
  -- check the kinds of the tyApp
  return $ replaceTypeVars [(tVar, ty)] fTy'
  where
  splitForall (Forall [x] ty) = (x, ty)
  splitForall (Forall (x:xs) ty) = (x, Forall xs ty)
  splitForall ty = error . show . pretty $ (ty, ap)
lintCore ap@(App f arg) = do
  fTy <- lintCore f
  argTy <- lintCore arg

  let (a, b) = getArgTy ap fTy
  if (isJust $ a `subsume` argTy) then return b
  else throwError $ "app error: " ++ (show $ pretty (fTy, argTy))
  where
  getArgTy _ (TAp (TAp (TConstructor "->") a) b) = (a, b)
  getArgTy _ (Arrow a b) = (a, b)
  -- getArgTy (Forall _ ty) = getArgTy ty
  getArgTy argTy ty' = error . show . pretty $ (argTy, ty')

lintCore c@(Case scrut alts) = do
  scrutTy <- lintCore scrut
  retTys <- mapM (lintAlt scrutTy) alts
  return $ head retTys

  -- figure out a situation for `failedPattern`
  -- if all (isJust . subsume (head retTys)) retTys
  -- then return $ head retTys
  -- else error "lol: implement alternative linting"
lintCore l@(Let bind exp) = do
  lintBind bind
  let NonRec b _ = bind
  t <- bindName b (lintCore exp)
  return t
lintCore (Type t) = do
  let vars = freeVariables t
  mapM_ lookupTyVar vars

  return t -- validate no unknown tyvars
lintCore (Lit lit) = return $ lintLit lit

lintLit (RawString _) = tString
lintLit (EscString _) = tString
lintLit (Integer _ )  = tInteger
lintLit (Double _)    = tDouble

lintAlt :: LintM m => Type String -> Alt Var -> m (Type String)
lintAlt ty (ConAlt i binds exp) = foldl (flip bindName) (lintCore exp) binds
lintAlt ty (TrivialAlt exp) = lintCore exp
lintAlt ty (LitAlt lit exp) = if lintLit lit == ty
  then lintCore exp
  else throwError "bad lit alt!"