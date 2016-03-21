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

  instance Pretty a => Pretty (Module a) where
    pretty (Module name decls) = text "module" <+> (text name) `aboveBreak` (nest 2 $ pretty decls) `aboveBreak` (text "end")

  instance Pretty (Cofree (Declaration a) a) where
  --  pretty (Data x1 x2) = _
  --  pretty (TypeSynonym x1 x2) = _
  --  pretty (Value x1 x2 x3 x4) = _
  --  pretty (Signature x1 x2) = _
    pretty (_ :< Import qual msk name alias) = do
      qual <- (when (const $ text "qualified") qual empty)
      mod  <- text name
      alias <- prettyJust alias
      masks <- prettyMask msk
      text "import" <+> qual <+> mod <+> alias <+> masks
        where prettyJust (Just alias) = text "as" <+> text alias
              prettyJust (Nothing)    = empty
              prettyMask (Hiding nms) = text "hiding" <+> (tupled $ map pretty nms)
              prettyMask (Only   nms) = tupled $ map pretty nms
              prettyMask _            = empty
  --  pretty (Trait x1 x2 x3 x4) = _

