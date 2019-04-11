{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE RecordWildCards       #-}
module Thrill.Desugar.LambdaLift where

import           Thrill.Syntax.Core
import           Thrill.Syntax.Kind
import           Thrill.Syntax.Name
import           Thrill.Syntax.Type
import           Thrill.Syntax.Builtins

import           Control.Monad.Fresh
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer

import           Data.List            ((\\))

import           Control.Applicative
import           Control.Monad

import           Thrill.Prelude

import           Data.Bifunctor

import Debug.Trace
import Thrill.Syntax.Pretty
{-
  1. Lift the MFE instead of naively lifting the whole body of a lambda
  2. Modularize lifter
    - Annotate Free vars (optional)
    - Expand lambdas
    - Lift to global scope
-}
data LiftingState = Lifted
  { boundNames  :: [QualifiedName]
  , boundTyVars :: [QualifiedName]
  , tyArgStack  :: [CoreExp]
  }
  deriving (Show)

type MonadLL m = (MonadReader Name m, MonadFresh m, MonadState LiftingState m, MonadWriter [Bind Var] m)

liftModule :: CoreModule -> CoreModule
liftModule m@Mod{..} = m { bindings = evalMonadStack $ mapM liftGlobal bindings }
  where
  evalMonadStack = uncurry (++) . evalFresh 0 . flip runReaderT coreModuleName . runWriterT . flip evalStateT (Lifted bindNames [] [])
  bindNames = map (\(NonRec v _) -> varName v) bindings ++ (map fst builtins) ++ consNames
  consNames = map fst constructors

liftBinding (NonRec nm exp) = do
  exp' <- liftLambda exp
  let ty = getTyOf exp'

  return $ NonRec (nm { idTy = ty}) exp'

liftGlobal :: MonadLL m => Bind Var -> m (Bind Var)
liftGlobal (NonRec nm l@(Lambda _ _)) = do
  exp' <- liftLambda f

  let liftedLam = foldr Lambda exp' args
  let ty = getTyOf liftedLam

  return $ NonRec (nm { idTy = ty}) liftedLam
  where
  (names, tyVars) = ([ varName x | x@Id{} <- args], [ varName x | x@TyVar{} <- args])
  (f, args) = unwrapLambda l

  unwrapLambda (Lambda a e) = (a :) <$> unwrapLambda e
  unwrapLambda e            = (e, [])
liftGlobal a = liftBinding a

pushTyArgs tys act = do
  modify $ \s -> s { tyArgStack = tys }
  r <- act

  modify $ \s -> s { tyArgStack = [] }
  return r

liftLambda :: MonadLL m => CoreExp -> m CoreExp
liftLambda (Let bind exp) = liftA2 Let (liftBinding bind) (liftLambda exp)
liftLambda app@(App _ (Type _)) = do
  let (func, tyargs) = unwindSpine app []

  func' <- pushTyArgs tyargs $ liftLambda func

  if isLambda func
  then pure func'
  else pure $ foldl App func' tyargs

  where
  unwindSpine (App func t@(Type _)) acc = unwindSpine func (t : acc)
  unwindSpine func acc = (func, acc)

  isLambda (Lambda _ _) = True
  isLambda _ = False
liftLambda (App f a) = liftA2 App (liftLambda f) (liftLambda a)
liftLambda (Case scrut alts) = liftA2 Case (liftLambda scrut) (liftCaseAlts alts)
  where liftCaseAlts = mapM liftCaseAlt
        liftCaseAlt (ConAlt id binds exp) = ConAlt id binds <$> liftLambda exp
        liftCaseAlt (LitAlt lit exp)      = LitAlt lit <$> liftLambda exp
        liftCaseAlt (TrivialAlt exp)      = TrivialAlt <$> liftLambda exp
liftLambda l@(Lambda _ _) = do
  let (inner, args, tyvars) = unwrapLambda l

  liftedBody <- liftLambda inner

  let liftedLam = foldr Lambda liftedBody (tyvars ++ args)
  (freeargs, freetvs) <- get >>= pure . runReader (freeVars liftedLam)

  l' <- closeVars (freeargs ++ args, freetvs ++ map varName tyvars) liftedBody

  lamVar <- emitLambda l'

  stack <- gets tyArgStack
  let typeVars = map (Type . TVar) (nub $ freetvs)
  return $ foldl App lamVar (typeVars ++ stack ++ map Var freeargs)
  where
  unwrapLambda :: CoreExp -> (CoreExp, [Var], [Var])
  unwrapLambda (Lambda a@(Id{}) e) = (\(e, as, ts) -> (e, a : as, ts)) $ unwrapLambda e
  unwrapLambda (Lambda t@(TyVar{}) e) = (\(e, a, ts) -> (e, a, t : ts)) $ unwrapLambda e
  unwrapLambda e            = (e, [], [])
liftLambda a = pure a

emitLambda exp = do
  nm <- (Qualified <$> ask <*> prefixedName "lifted")
  let var = Id nm (getTyOf exp) Used
  modify $ \s -> s { boundNames = nm : boundNames s }
  tell [NonRec var exp]
  return (Var var)

type FreeVars =
  ( [Var]            -- arguments
  , [QualifiedName]  -- tyvars
  )

-- need to handle type variaxbles smoothly
freeVars :: MonadReader LiftingState m => CoreExp -> m FreeVars
freeVars (Lambda n@TyVar{} exp) = local (\s -> s { boundTyVars = varName n : boundTyVars s }) (freeVars exp)
freeVars (Lambda n@Id{} exp) = do
  binderFVs <- asks (\y -> freeVariables (idTy n) \\ boundTyVars y)
  bodyFVs <- local (\s -> s { boundNames = varName n : boundNames s }) (freeVars exp)

  return $ fmap (binderFVs ++) bodyFVs
freeVars (App f a) = liftA2 (<>) (freeVars f) (freeVars a)
freeVars (Case core alts) = liftA2 (<>) (freeVars core) (foldl1 (<>) <$> forM alts freeAltVar)
  where
  freeAltVar (ConAlt _ binds exp) = local
    (\s -> s { boundNames = map varName binds ++ boundNames s }) $ freeVars exp
  freeAltVar (TrivialAlt exp) = freeVars exp
  freeAltVar (LitAlt _ exp) = freeVars exp
freeVars (Var v) = do
  isBound <- asks (elem (varName v) . boundNames)

  if isBound
  then pure ([ ], [])
  else pure ([v], [])
freeVars (Let (NonRec n exp) letExp) = do
  binderFVs <- asks (\y -> freeVariables (idTy n) \\ boundTyVars y)

  expFVs <- local (\s -> s { boundNames = varName n : boundNames s }) $
    liftA2 (<>) (freeVars exp) (freeVars letExp)

  return $ fmap (binderFVs ++) expFVs
freeVars (Type t) = do
  freeVars <- asks (\y -> freeVariables t \\ boundTyVars y)
  pure ([], freeVars)
freeVars (Lit _)  = pure ([], [])

closeVars :: MonadLL m => FreeVars -> CoreExp -> m CoreExp
closeVars (vars, tyVars) exp = do
  names <- replicateM (length vars) (Internal <$> prefixedName "cvar")

  let vars' = zipWith (\nm var -> (varName var, var { varName = nm })) names vars
      exp'  = foldr (\(i, v) exp -> Lambda v $ substitute (i, Var v) exp) exp vars'
      exp'' = foldr (Lambda . ((`TyVar` Star))) exp' (nub tyVars)
  return exp''
