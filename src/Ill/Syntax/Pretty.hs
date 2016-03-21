module Ill.Syntax.Pretty
( module Text.PrettyPrint.Free
, parensIf
, when
) where

  import Text.PrettyPrint.Free

  parensIf :: Bool -> Doc a -> Doc a
  parensIf = when parens

  when :: (Doc a -> Doc a) -> Bool -> Doc a -> Doc a
  when t c doc = if c then t doc else doc
