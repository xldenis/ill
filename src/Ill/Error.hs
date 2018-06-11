module Ill.Error
  ( module Ill.Error
  , module Control.Monad.Except
  ) where

import           Ill.Prelude

import           Ill.Syntax (Type, Name, Kind, Constraint, Expr, TypedAnn, SourceSpan, Pat)
import           Control.Monad.Except
import           Ill.Syntax.Pretty

import Control.Comonad (extract)

{-
  Taken from https://github.com/jaspervdj/talks/blob/master/2017-skillsmatter-errors/slides.md#which-is-the-best-representation-2

  Should probably migrate to a custom annotation type that is then interpreted into the relevant
  pretty printer backend annotation. Would also help standardize the specific styles that are
  used.

-}

type Error' = Error AnsiStyle

data Error a = Error
  { errHeader  :: Doc a -- name of error (subject)
  , errKind    :: String -- subsystem (sender)
  , errSummary :: Doc a -- details of error (body)
  , errHints   :: [Doc a] -- solutions
  }

prettyError :: Error AnsiStyle -> Doc AnsiStyle
prettyError err = nest 2 $ (annotate bold $ errHeader err)
    `above` (errSummary err)
    `above` bulleted (map (\d -> align $ hint<+> d) (errHints err))
  where
  hint = annotate (color Magenta <> bold) (pretty "hint:")
rethrow :: MonadError e m => (e -> e) -> m a -> m a
rethrow f action = action `catchError` (throwError . f )

