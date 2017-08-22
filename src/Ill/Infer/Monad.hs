{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Ill.Infer.Monad where

import           Ill.Error
import           Ill.Parser.Lexer     (SourceSpan (..))
import           Ill.Syntax

import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Unify

data Environment = Environment
  { names             :: [(Name, Type Name)]
  , types             :: [(Name, Kind)]
  , constructors      :: [(Name, (Name, Type Name, [Name]))]
  , traits            :: TraitDict
  , traitDictionaries :: InstanceDict
  } deriving (Show, Eq)

type TraitDict = [(Name, TraitEntry)]
type InstanceDict = [(Name, [TraitInstance])]

type TraitInstance = ([Type Name], [Constraint Name])
type TraitEntry = ([Constraint Name], [Name], [(Name, Type Name)])

data CheckState = CheckState
  { env     :: Environment
  , nextVar :: Int
  } deriving (Show, Eq)

newtype Check a = Check { runCheck :: StateT CheckState (Except MultiError) a}
  deriving (Functor, Applicative, Monad, MonadError MultiError, MonadState CheckState)

data TypedAnn = Ann { span :: SourceSpan, ty :: TypeAnn }
  deriving (Show, Eq)

data TypeAnn
  = Type (Type Name)
  | Kind Kind
  | None
  deriving (Show, Eq)

defaultCheckEnv = CheckState (Environment
  [ ("+", tInteger `tFn` tInteger `tFn` tInteger)
  , ("-", tInteger `tFn` tInteger `tFn` tInteger)
  , ("*", tInteger `tFn` tInteger `tFn` tInteger)
  , ("/", tInteger `tFn` tInteger `tFn` tInteger)
  , (">", tInteger `tFn` tInteger `tFn` tBool)
  ] [] [] [] []) 0

putEnv :: MonadState CheckState m => Environment -> m ()
putEnv e = modify (\s -> s { env  = e })

getEnv :: MonadState CheckState m => m Environment
getEnv = env <$> get

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
  env <- getEnv
  case lookup name (names env) of
    Nothing -> throwError $ UndefinedVariable name
    Just a  -> return a

lookupConstructor :: (MonadError MultiError m, MonadState CheckState m) => Name -> m (Name, Type Name, [Name])
lookupConstructor name = do
  env <- getEnv
  case lookup name (constructors env) of
    Nothing -> throwError $ UndefinedConstructor name
    Just a  -> return a

lookupTypeVariable ::  (MonadError MultiError m, MonadState CheckState m) => Name -> m Kind
lookupTypeVariable name = do
  env <- getEnv
  case lookup name (types env) of
    Nothing -> throwError $ UndefinedType name
    Just a  -> return a

lookupTrait :: (MonadError MultiError m, MonadState CheckState m) => Name -> m TraitEntry
lookupTrait name = do
  env <- getEnv
  case lookup name (traits env) of
    Nothing -> throwError $ UndefinedTrait name
    Just a  -> return a

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

addTrait :: MonadState CheckState m => Name -- class name
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

withTraitInstance :: MonadState CheckState m => Name -> [Constraint Name] -> [Type Name] -> m a -> m a
withTraitInstance trait supers inst action = do
  env <- env <$> get

  let instDict = case trait `lookup` (traitDictionaries env) of
                  Just instances -> (inst, supers) : instances
                  Nothing        -> [(inst, supers)]

  putEnv $ env { traitDictionaries = replace (trait, instDict) (traitDictionaries env) }
  a <- action

  modifyEnv (\env -> env { traitDictionaries = (traitDictionaries env) })

  return a

  where

  replace (k1, v) ((k2, _): xs) | k1 == k2 = (k1, v) : xs
  replace v (x : xs)            = x : replace v xs
  replace v []                  = [v]

addTraitInstance :: Name -> [Constraint Name] -> [Type Name] -> Check ()
addTraitInstance trait supers inst = do
  env <- env <$> get

  let instDict = case trait `lookup` (traitDictionaries env) of
                  Just instances -> (inst, supers) : instances
                  Nothing        -> [(inst, supers)]

  putEnv $ env { traitDictionaries = replace (trait, instDict) (traitDictionaries env) }

  where

  replace (k1, v) ((k2, _): xs) | k1 == k2 = (k1, v) : xs
  replace v (x : xs)            = x : replace v xs
  replace v []                  = [v]

