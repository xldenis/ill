module Ill.Error
  ( module Ill.Error
  , module Control.Monad.Except
  ) where

import           Ill.Syntax (Type, Name, Kind, Constraint, Expr, TypedAnn, SourceSpan)
import           Control.Monad.Except
import           Ill.Syntax.Pretty

import Control.Comonad (extract)
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
  | ErrorInExpression (Expr SourceSpan) (MultiError)
  deriving (Show, Eq)

internalError :: (MonadError MultiError m) => String -> m ()
internalError = throwError . InternalError

notImplementedError :: (MonadError MultiError m) => String -> m ()
notImplementedError = throwError . NotImplementedError

rethrow :: MonadError e m => (e -> e) -> m a -> m a
rethrow f action = action `catchError` (throwError . f )

instance Pretty MultiError where
  pretty (InternalError s) = pretty "internal error" <+> pretty s
  pretty (UnificationError t1 t2) = pretty "Unification error could not" <+> hang 1 doc
    where doc = vsep [pretty "unify:" <+> pretty t1, pretty "with:" <+> pretty t2]
  pretty (ErrorInExpression location error) = vcat $
    [ pretty "Error in the expression at" <+> pretty (extract location) <> pretty ":"
    , pretty location
    , (nest 2 $ pretty error)
    ]

  pretty s = pretty $ show s
