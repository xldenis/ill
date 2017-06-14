{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ill.Infer.Monad where

import Ill.Syntax (Name, Type)

import Control.Monad.State
import Control.Monad.Unify
import Control.Monad.Except

data Kind = Kind
  deriving (Show, Eq)

data Environment = Environment
  { names :: [(Name, Type Name)]
  , types :: [(Name, Kind)]
  } deriving (Show, Eq)

data CheckState = CheckState
  { env :: Environment
  , nextVar :: Int
  } deriving (Show, Eq)

newtype Check a = Check { runCheck :: StateT CheckState (Except String) a}
  deriving (Functor, Applicative, Monad, MonadError String, MonadState CheckState)

defaultCheckEnv = CheckState (Environment [] []) 0

localState :: MonadState s m => (s -> s) -> m a -> m a
localState f action = do
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
  let uust = unifyCurrentSubstitution ust
  return (a, uust)
