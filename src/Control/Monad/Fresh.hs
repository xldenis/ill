{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Control.Monad.Fresh where

import Control.Monad.State
import Control.Monad.Reader
{-
  A simple type class for monads which can produce fresh names

-}

class Monad m => MonadFresh m where
  freshName :: m Int

newtype FreshT m a = FreshT { unFreshT :: StateT Int m a }
  deriving (Functor, Applicative, Monad, MonadState Int, MonadTrans)

instance MonadFresh m => MonadFresh (StateT s m) where
  freshName = lift freshName

instance MonadFresh m => MonadFresh (ReaderT s m) where
  freshName = lift freshName

instance Monad m => MonadFresh (FreshT m) where
  freshName = do
    next <- get
    modify (+ 1)
    return next
