{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE DeriveDataTypeable         #-}
module Ill.Infer.Monad where

import           Ill.Prelude

import           Ill.Error
import           Ill.Parser.Lexer     (SourceSpan (..))
import           Ill.Syntax

import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Unify

import           Ill.Syntax.Builtins
import           Data.Map (Map, insert, union, fromList, (!?), (!), insertWith, adjust)

import           Data.Data

data Environment = Environment
  { names             :: Map Name (Type Name)
  , types             :: Map Name Kind
  , constructors      :: Map Name ConstructorEntry
  , traits            :: TraitDict
  , traitDictionaries :: InstanceDict
  } deriving (Show, Eq)

type TraitDict = [(Name, TraitEntry)]

data ConstructorEntry = ConstructorEntry
  { consName :: Name
  , consType :: Type Name
  , consTyVars :: [Name]
  , consArity :: Int
  , consTag :: Int
  } deriving (Show, Eq, Data)

data TraitEntry = TraitEntry
  { superTraits :: [Constraint Name]
  , traitVar  :: Name
  , methodSigs :: [(Name, Type Name)]
  } deriving (Show, Eq)

type InstanceDict = Map Name [InstanceEntry] -- [(Name, [InstanceEntry])]

data InstanceEntry = InstanceEntry
  { instType        :: Type Name
  , instConstraints :: [Constraint Name]
  , instName        :: Name
  } deriving (Show, Eq)

data CheckState = CheckState
  { env     :: Environment
  , nextVar :: Int
  } deriving (Show, Eq)

newtype Check a = Check { runCheck :: StateT CheckState (Except MultiError) a}
  deriving (Functor, Applicative, Monad, MonadError MultiError, MonadState CheckState)

execCheck c = runExcept $ flip runStateT defaultCheckEnv $ (runCheck c)

defaultCheckEnv = CheckState (Environment (fromList
  builtins) mempty mempty mempty mempty) 0

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
  let ut = runUnify (defaultUnifyState { unifyNextVar = nextVar st}) action
  (a, ust) <- ut
  modify $ \st -> st { nextVar = unifyNextVar ust }
  let uust = unifyCurrentSubstitution ust
  return (a, uust)

lookupVariable :: (MonadError MultiError m, MonadState CheckState m) => Name -> m (Type Name)
lookupVariable name = do
  env <- getEnv
  case names env !? name of
    Nothing -> throwError $ UndefinedVariable name
    Just a  -> return a

lookupConstructor :: (MonadError MultiError m, MonadState CheckState m) => Name -> m ConstructorEntry
lookupConstructor name = do
  env <- getEnv
  case constructors env !? name of
    Nothing -> throwError $ UndefinedConstructor name
    Just a  -> return a

lookupTypeVariable ::  (MonadError MultiError m, MonadState CheckState m) => Name -> m Kind
lookupTypeVariable name = do
  env <- getEnv
  case types env !? name of
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
  modify (\s -> s { env = (env s) { names = (fromList nms) `union` names (env s) } })
  a <- action
  modify (\s -> s { env = (env s) { names = names . env $ orig } })
  return a

bindTypeVariables :: MonadState CheckState m => [(Name, Kind)] -> m a -> m a
bindTypeVariables tyVars action = do
  orig <- get
  modify (\s -> s { env = (env s) { types = types (env s) `union` (fromList tyVars) } })
  a <- action
  modify (\s -> s { env = (env s) { types = types . env $ orig } })
  return a

addNames bound = modifyEnv $ \e -> e { names = fromList bound `union` names e }

modifyEnv f = modify $ \st -> st { env = f (env st) }

addTrait :: MonadState CheckState m => Name -- class name
  -> [Constraint Name] -- super classes
  -> Name -- variables of class
  -> [(Name, Type Name)]  -- name and type of member sigs
  -> m ()
addTrait name supers arg members = do
  let qualifiedMembers = map (fmap (generalize . qualifyType)) members
  modifyEnv $ \e -> e { traits = (name, TraitEntry supers arg qualifiedMembers) : traits e }
  modifyEnv $ \e -> e { names = (fromList qualifiedMembers) `union` names e }
  where
  qualifyType t = Constrained fullConstraints t
  fullConstraints = (name, TVar arg) : supers

withTraitInstance :: MonadState CheckState m => Name -> [Constraint Name] -> Type Name -> m a -> m a
withTraitInstance trait supers inst action = do
  environment <- env <$> get

  putEnv $ environment { traitDictionaries = insertWith (++) trait [InstanceEntry inst supers trait] (traitDictionaries environment) }
  a <- action

  modifyEnv (\env' -> env' { traitDictionaries = traitDictionaries environment })

  return a

addTraitInstance :: Name -> [Constraint Name] -> Type Name -> Check ()
addTraitInstance trait supers inst = do
  env <- env <$> get

  putEnv $ env { traitDictionaries = insertWith (++) trait [InstanceEntry inst supers trait] (traitDictionaries env) }

addValue :: Name -> Type Name -> Check ()
addValue name ty = do
  env <- env <$> get
  let env' = env { names = insert name ty (names env)  }
  modify $ \s -> s { env = env' }

addDataType :: Name -> [Name] -> [(Name, [Type Name])] -> Kind -> Check ()
addDataType name args dctors ctorKind = do
  env <- env <$> get
  let value = ctorKind
  let env' = env { types = insert name ctorKind (types env) }
  modify $ \s -> s { env = env' }

  zipWithM_ (\i -> uncurry $ addDataConstructor name args i ) [0..] dctors

addDataConstructor :: Name -> [Name] -> Int -> Name -> [Type Name] -> Check ()
addDataConstructor tyCons args tag dataCons tys = do
  env <- env <$> get
  let retTy = foldl TAp (TConstructor tyCons) (map TVar args)
      dataConsTy = generalize $ foldr tFn retTy tys
      fields = args
      consEntry = ConstructorEntry tyCons dataConsTy fields (length tys) tag
  putEnv $ env { constructors = insert dataCons consEntry (constructors env) }
  return ()
