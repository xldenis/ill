module Ill.Syntax.Pretty
( module Text.PrettyPrint.Free
, parensIf
) where

  import Text.PrettyPrint.Free

  parensIf :: Bool -> Doc a -> Doc a
  parensIf b doc = if b then parens doc else doc
