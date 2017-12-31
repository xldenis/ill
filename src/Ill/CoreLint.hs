{-# LANGUAGE ConstraintKinds #-}
module Ill.CoreLint
( runLinter
) where

import           Ill.Prelude

import           Ill.Syntax.Core
import           Ill.Syntax.Literal
import           Ill.Syntax.Pretty
import           Ill.Syntax.Type

import           Control.Monad.Except
import           Control.Monad.State

-- fix this... somehow, primitives are leaking all over the place
import           Ill.Infer.Monad     (defaultCheckEnv, env, names)

import qualified Data.Map            as M
import           Data.Maybe

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
  { boundNames  :: M.Map String (Type String)
  , boundTyVars :: [String]
  } deriving (Show, Eq)

runLinter :: CoreModule -> Either String ()
runLinter = flip evalState (E (names $ env defaultCheckEnv) []) . runExceptT . lintModule
  where
  lintModule mod = bindNames (map getVar $ bindings mod) $
    bindCons (constructors mod) (mapM_ lintBind (bindings mod))

  getVar (NonRec v _) = v

  bindCons conses action = do
    let x = M.fromList $ map (\(c, (_, ty)) -> (c, ty)) conses
    orig <- get
    modify (\s -> s { boundNames = x `M.union` boundNames orig })
    val <- action
    modify (\s -> s { boundNames = boundNames orig })
    return val

bindNames :: LintM m => [Var] -> m a -> m a
bindNames vars act =
  foldr bindName act vars

bindName :: (LintM m) => Var -> m a -> m a
bindName v@Id{} action = do
  orig <- get
  modify $ \s -> s  { boundNames = M.insert (varName v) (idTy v) (boundNames orig) }
  val <- action
  modify (\s -> s { boundNames = boundNames orig })
  return val
bindName v@TyVar{} action = do
  orig <- get
  modify (\s -> s  { boundTyVars = varName v : boundTyVars orig })
  val <- action
  modify (\s -> s { boundTyVars = boundTyVars orig })
  return val

lintBind :: (LintM m) => Bind Var -> m ()
lintBind (NonRec b exp) = do
  ty <- bindName b (lintCore exp)

  case (idTy b) == ty of
    True -> pure ()
    False -> throwError $ "the type of binding " ++ show (pretty ty) ++ " did not match expected type " ++ show (pretty $ idTy b)

lookupName var = do
  names <- gets boundNames
  case var `M.lookup` names of
    Just x  -> return x
    Nothing -> throwError $ "the name " ++ show var ++ " could not be found."

lookupTyVar var = do
  tyvars <- gets boundTyVars
  if var `elem` tyvars then return () else throwError $ "the typevariable " ++ show var ++ " could not be found."

-- Check the types of the core expression
lintCore :: LintM m => CoreExp -> m (Type String)
lintCore l@(Lambda bind exp) = do
  bodyTy <- bindName bind (lintCore exp)
  return $ makeCorrectFunTy bind bodyTy
  where
  makeCorrectFunTy id@Id{} bodyTy    =  idTy bind `tFn` bodyTy
  makeCorrectFunTy tv@TyVar{} bodyTy =  case bodyTy of
    Forall vars ty -> Forall (varName tv : vars) ty
    ty             -> Forall [varName tv] ty
  -- need to build a type lambda instead of a term lambda
lintCore (Var var) = lookupName (varName var)
lintCore ap@(App f t@(Type ty)) = do
  lintCore t
  fTy <- lintCore f

  let (tVar, fTy') = splitForall fTy
  -- check the kinds of the tyApp
  return $ replaceTypeVars [(tVar, ty)] fTy'
  where
  splitForall (Forall [x] ty) = (x, ty)
  splitForall (Forall (x:xs) ty) = (x, Forall xs ty)
  splitForall ty = error . show . vcat $
    [ pretty "type variable applied to non polymorphic function!"
    , pretty (ty, ap)
    ]
lintCore ap@(App f arg) = do
  fTy <- lintCore f
  argTy <- lintCore arg

  (a, b) <- getArgTy ap fTy
  if isJust $ a `subsume` argTy then return b
  else throwError $ "app error: " ++ show (pretty (fTy, argTy))
  where
  getArgTy _ (TAp (TAp (TConstructor "->") a) b) = pure (a, b)
  getArgTy _ (Arrow a b) = pure (a, b)
  getArgTy _ ty' = throwError . show $ vcat
    [ pretty "The function has an unresolved polymorphic variable in the application:"
    , pretty ap
    , pretty "function:"
    , pretty f
    , pretty "function type:"
    , pretty ty'
    ]

lintCore c@(Case scrut alts) = do
  scrutTy <- lintCore scrut
  retTys <- mapM (lintAlt scrutTy) alts
  return $ head retTys

  if all (isJust . subsume (head retTys)) retTys
  then return $ head retTys
  else throwError . show $ pretty "hmmm some alts done have the same types!" <+> pretty retTys
lintCore l@(Let bind exp) = do
  lintBind bind
  let NonRec b _ = bind
  bindName b (lintCore exp)
lintCore (Type t) = do
  let vars = freeVariables t
  mapM_ lookupTyVar vars

  return t -- validate no unknown tyvars
lintCore (Lit lit) = return $ litType lit

lintAlt :: LintM m => Type String -> Alt Var -> m (Type String)
lintAlt ty (ConAlt i binds exp) = foldl (flip bindName) (lintCore exp) binds
lintAlt ty (TrivialAlt exp) = lintCore exp
lintAlt ty (LitAlt lit exp) = if litType lit == ty
  then lintCore exp
  else throwError "bad lit alt!"
