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

  type Expr a = Cofree Expression a

  -- instance Pretty (Module a) where
  --  pretty (Module name decls) = text "module" <+> (text name) <$> (nest 2 $ pretty decls) <$> (text "end")

  -- instance Pretty (Declaration a b) where
  --  pretty (Data x1 x2) = _
  --  pretty (TypeSynonym x1 x2) = _
  --  pretty (Value x1 x2 x3 x4) = _
  --  pretty (Signature x1 x2) = _
  --  pretty (Import x1 x2 x3 x4) = _
  --  pretty (Trait x1 x2 x3 x4) = _

