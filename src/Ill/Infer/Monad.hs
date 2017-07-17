{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ill.Infer.Monad where

import Ill.Syntax.Type
import Ill.Syntax.Kind
import Ill.Syntax (Name)
import Ill.Error
import Control.Monad.State
import Control.Monad.Unify
import Control.Monad.Except

data Environment = Environment
  { names :: [(Name, Type Name)]
  , types :: [(Name, Kind)]
  , constructors :: [(Name, (Name, Type Name, [Name]))]
  , traits :: [(Name, ClassEntry)]
  } deriving (Show, Eq)

type ClassEntry = ([Constraint Name], [Name], [(Name, Type Name)])

data CheckState = CheckState
  { env :: Environment
  , nextVar :: Int
  } deriving (Show, Eq)

newtype Check a = Check { runCheck :: StateT CheckState (Except MultiError) a}
  deriving (Functor, Applicative, Monad, MonadError MultiError, MonadState CheckState)

defaultCheckEnv = CheckState (Environment
  [ ("+", tInteger `tFn` tInteger `tFn` tInteger)
  , ("-", tInteger `tFn` tInteger `tFn` tInteger)
  , ("*", tInteger `tFn` tInteger `tFn` tInteger)
  , ("/", tInteger `tFn` tInteger `tFn` tInteger)
  , (">", tInteger `tFn` tInteger `tFn` tBool)
  ] [] [] []) 0

putEnv :: MonadState CheckState m => Environment -> m ()
putEnv e = modify (\s -> s { env  = e })

localState :: MonadState s m => (s -> s) -> m a -> m a
localState f action = do -- get rid of this it doesn't work
  orig <- get
  modify f
  a <- action
  put orig
  return a

liftUnify :: Partial t => UnifyT t Check a -> Check (a, Substitution t)
liftUnify action = do
  st <- get
  let ut = runUnify (defaultUnifyState { unifyNextVar = (nextVar st)}) action
  (a, ust) <- ut
  modify $ \st -> st { nextVar = unifyNextVar ust }
  let uust = unifyCurrentSubstitution ust
  return (a, uust)

lookupVariable :: (MonadError MultiError m, MonadState CheckState m) => Name -> m (Type Name)
lookupVariable name = do
  env <- env <$> get
  case lookup name (names env) of
    Nothing -> throwError $ UndefinedVariable name
    Just a -> return a

lookupConstructor :: (MonadError MultiError m, MonadState CheckState m) => Name -> m (Name, Type Name, [Name])
lookupConstructor name = do
  env <- env <$> get
  case lookup name (constructors env) of
    Nothing -> throwError $ UndefinedConstructor name
    Just a -> return a

lookupTypeVariable ::  (MonadError MultiError m, MonadState CheckState m) => Name -> m Kind
lookupTypeVariable name = do
  env <- env <$> get
  case lookup name (types env) of
    Nothing -> throwError $ UndefinedType name
    Just a -> return a

lookupTrait :: (MonadError MultiError m, MonadState CheckState m) => Name -> m ClassEntry
lookupTrait name = do
  env <- env <$> get
  case lookup name (traits env) of
    Nothing -> throwError $ UndefinedTrait name
    Just a -> return a

bindNames :: MonadState CheckState m => [(Name, Type Name)] -> m a -> m a
bindNames nms action = do
  orig <- get
  modify (\s -> s { env = (env s) { names = (names $ env s) ++ nms } })
  a <- action
  modify (\s -> s { env = (env s) { names = names . env $ orig } })
  return a

bindTypeVariables :: MonadState CheckState m => [(Name, Kind)] -> m a -> m a
bindTypeVariables tyVars action = do
  orig <- get
  modify (\s -> s { env = (env s) { types = (types $ env s) ++ tyVars } })
  a <- action
  modify (\s -> s { env = (env s) { types = types . env $ orig } })
  return a

modifyEnv f = modify $ \st -> st { env = f (env st) }

addTrait :: MonadState CheckState m => Name  -- class name
  -> [Constraint Name] -- super classes
  -> [Name] -- variables of class
  -> [(Name, Type Name)]  -- name and type of member sigs
  -> m ()
addTrait name supers args members = do
  modifyEnv $ \e -> e { traits = (name, (supers, args, members)) : (traits e) }
  let qualifiedMembers = map (fmap qualifyType) members
  modifyEnv $ \e -> e { names = qualifiedMembers ++ (names e) }
  where
  qualifyType t = Constrained fullConstraints t
  fullConstraints = (name, (map TVar args)) : supers
