{-# LANGUAGE DeriveFunctor, DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}

module Ill.Syntax
( module Ill.Syntax.Type
, module Ill.Syntax.Literal
, module Ill.Syntax.Expression
, module Ill.Syntax.Pattern
, module Ill.Syntax
) where

  import Control.Comonad.Cofree

  import Ill.Syntax.Type
  import Ill.Syntax.Literal
  import Ill.Syntax.Pattern
  import Ill.Syntax.Expression

  import Ill.Syntax.Pretty

  type Prefix = String

  type Qualified = Bool

  type Name = String

  type Alias = Maybe String

  data Module a = Module Name [Decl a] deriving (Show)

  data Declaration a b
    = Data Name [Type]
    | TypeSynonym Type Type
    | Value Name [([Pattern], [Expr a])]
    | Signature Name Type
    | Import Qualified Masks String Alias
    | TraitDecl Type [b]
    | TraitImpl Type
    deriving (Functor, Show)

  type Decl a = Cofree (Declaration a) a

  data Masks
    = Hiding [Name]
    | Only  [Name]
    | All
    deriving (Show)

  instance Pretty (Module a) where
    pretty (Module name decls) = (nest 2 $ text "module" <+> (text name) `aboveBreak` (vsep $ map pretty decls)) `aboveBreak` (text "end")

  instance Pretty (Cofree (Declaration a) a) where
    pretty (_ :< Data name sum) = text "data" <+> text name <+> (char '=') <+> alternative (map pretty sum)
      where alternative = encloseSep empty empty (char '|')
    pretty (_ :< TypeSynonym alias target) = text "type" <+> pretty alias <+> text "=" <+> pretty target
    pretty (_ :< Value name cases) = text "fn" <+> text name <+> (branch $ head cases) `aboveBreak` (vsep $ map (\c -> text "or" <+> text name <+> branch c) $ tail cases) `aboveBreak` (text "end")
      where branch (args, body) = nest 2 $ (tupled $ map pretty args) `aboveBreak` (vsep $ map pretty body)
    pretty (_ :< Signature func tp) = text func <+> text "::" <+> pretty tp
    pretty (_ :< Import qual msk name alias) = do
      text "import" <-> do
        (when (const $ text "qualified") qual empty)
      <-> text name <-> do
        prettyJust alias <-> prettyMask msk
        where prettyJust (Just alias) = text "as" <+> text alias
              prettyJust (Nothing)    = empty
              prettyMask (Hiding nms) = text "hiding" <+> (tupled $ map pretty nms)
              prettyMask (Only   nms) = tupled $ map pretty nms
              prettyMask _            = empty
    pretty (_ :< TraitDecl trt body) = do
      nest 2 (text "trait" <+> (pretty trt) `above` (vsep $ map pretty body)) `above` (text "end")
      where constraints c = if null c then empty else hsep (punctuate comma (map pretty c)) <+> (text "|")
