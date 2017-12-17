{-# LANGUAGE NamedFieldPuns #-}
module Ill.Syntax.Pretty
( module Data.Text.Prettyprint.Doc
, module Ill.Syntax.Pretty
, intersperse
) where

import Ill.Prelude

import Data.Text.Lazy hiding (intersperse)
import Data.Text.Prettyprint.Doc.Render.Text

import Data.Text.Prettyprint.Doc hiding (width)
import Data.Text.Prettyprint.Doc.Internal (Doc(..), PageWidth(..))

-- Use this from upstream when released
class Pretty1 f where
    liftPretty
        :: (a -> Doc ann)
        -> f a
        -> Doc ann

parensIf :: Bool -> Doc a -> Doc a
parensIf = conditionally parens

conditionally :: (Doc a -> Doc a) -> Bool -> Doc a -> Doc a
conditionally t c doc = if c then t doc else doc

data RenderArgs = RenderArgs {ribbon :: Double, width :: Int}

defaultRenderArgs :: RenderArgs
defaultRenderArgs = RenderArgs {ribbon = 1.0, width = 100}

renderIll :: RenderArgs -> Doc a -> Text
renderIll RenderArgs{ribbon , width} doc = renderLazy (layoutPretty (LayoutOptions $ AvailablePerLine width ribbon) doc)

renderIll' :: Doc a -> Text
renderIll' = renderIll defaultRenderArgs

{-
  A helper method for the purpose of determining whether a pretty document should be wrapped in parens
-}
complexDoc :: Doc a -> Bool
complexDoc Empty = False
complexDoc (Char _) = False
complexDoc (Text _ _) = False
complexDoc (Annotated _ d) = complexDoc d
complexDoc _ = True

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
