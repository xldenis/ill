{-# LANGUAGE NamedFieldPuns #-}
module Ill.Syntax.Pretty
( module Data.Text.Prettyprint.Doc
, module Ill.Syntax.Pretty
, intersperse
) where


import Data.Text.Lazy hiding (intersperse)
import Data.Text.Prettyprint.Doc.Render.Text

import Data.Text.Prettyprint.Doc hiding (width)
import Data.Text.Prettyprint.Doc.Internal (Doc(..), PageWidth(..))
import Data.List (intersperse)

parensIf :: Bool -> Doc a -> Doc a
parensIf = when parens

when :: (Doc a -> Doc a) -> Bool -> Doc a -> Doc a
when t c doc = if c then t doc else doc

data RenderArgs = RenderArgs {ribbon :: Double, width :: Int}

defaultRenderArgs :: RenderArgs
defaultRenderArgs = RenderArgs {ribbon = 1.0, width = 100}

renderIll :: RenderArgs -> Doc a -> Text
renderIll RenderArgs{ribbon , width} doc = renderLazy (layoutPretty (LayoutOptions $ AvailablePerLine width ribbon) doc)

renderIll' :: Doc a -> Text
renderIll' = renderIll defaultRenderArgs

(<->) :: Doc a -> Doc a -> Doc a
a <-> Empty = a
Empty <-> a = a
a <-> b = a <+> b

(</>) :: Doc a -> Doc a -> Doc a
a </> b = fillSep [a, b]

above :: Doc a -> Doc a -> Doc a
above a b = vsep [a, b]

vsep' :: [Doc ann] -> Doc ann
vsep' = concatWith (\x y -> x <> hardline <> y)

text :: Text -> Doc a
text = pretty
