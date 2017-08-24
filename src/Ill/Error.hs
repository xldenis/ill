module Ill.Error
  ( module Ill.Error
  , module Control.Monad.Except
  ) where

import           Ill.Syntax (Type, Name, Kind, Constraint)
import           Control.Monad.Except
import           Ill.Syntax.Pretty

data MultiError
  = UnificationError (Type Name) (Type Name)
  | InternalError String
  | UndefinedType String
  | UndefinedTrait String
  | UndefinedVariable String
  | UndefinedConstructor String
  | NotImplementedError String
  | KindUnificationError Kind Kind
  | KindOccursError Kind
  | TypeOccursError (Type Name)
  | MissingTraitImpl [Constraint Name]
  deriving (Show, Eq)

internalError :: (MonadError MultiError m) => String -> m ()
internalError = throwError . InternalError

instance Pretty MultiError where
  pretty (InternalError s) = pretty "internal error" <+> pretty s
  pretty s = pretty $ show s