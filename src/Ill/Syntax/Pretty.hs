{-# LANGUAGE NamedFieldPuns #-}
module Ill.Syntax.Pretty
( module Text.PrettyPrint.Free
, parensIf
, when
, renderIll
, defaultRenderArgs
, (<->)
) where

import Text.PrettyPrint.Free hiding (width)
import Text.PrettyPrint.Free.Internal (Doc(..))

parensIf :: Bool -> Doc a -> Doc a
parensIf = when parens

when :: (Doc a -> Doc a) -> Bool -> Doc a -> Doc a
when t c doc = if c then t doc else doc

data RenderArgs = RenderArgs {ribbon :: Float, width :: Int}

defaultRenderArgs :: RenderArgs
defaultRenderArgs = RenderArgs {ribbon = 1.0, width = 100}

renderIll :: RenderArgs -> Doc a -> String
renderIll RenderArgs{ribbon , width} doc = displayS (renderPretty ribbon width doc) ""

(<->) :: Doc a -> Doc a -> Doc a
a <-> Empty = a
Empty <-> a = a
a <-> b = a <+> b
