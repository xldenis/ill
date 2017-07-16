module Ill.Error
  ( module Ill.Error
  , module Control.Monad.Except
  ) where

import Ill.Syntax (Type, Name, Kind)
import           Control.Monad.Except

data MultiError
  = UnificationError (Type Name) (Type Name)
  | InternalError String
  | UndefinedType String
  | UndefinedVariable String
  | UndefinedConstructor String
  | NotImplementedError String
  | KindUnificationError Kind Kind
  | KindOccursError Kind
  | TypeOccursError (Type Name)
  deriving (Show, Eq)

internalError :: MonadError MultiError m => String -> m ()
internalError = throwError . InternalError