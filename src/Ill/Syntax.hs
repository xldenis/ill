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
    | Value Name (Maybe Type) [Pattern] [Expr a]
    | Signature Name Type
    | Import Qualified Masks String Alias
    | Trait [Type] Name Type [a]
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
  --  pretty (Data x1 x2) = _
    pretty (_ :< TypeSynonym alias target) = text "type" <+> pretty alias <+> text "=" <+> pretty target
    --pretty (_ :< Value name ret args body) = text "fn" <+> text name <+> (tupled $ map pretty args) <+>
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
  --  pretty (Trait x1 x2 x3 x4) = _

