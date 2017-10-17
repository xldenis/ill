module Ill.Syntax.Core where

import Ill.Syntax.Pretty
import Control.Lens.Extras (is)

import Ill.Syntax.Type
import Ill.Syntax.Kind
import Ill.Syntax.Literal
import Ill.Syntax.Name

data Core n
  = Lambda n (Core n)
  | App (Core n) (Arg n)
  | Case (Core n) [Alt n]
  | Var Id
  | Let (Bind n) (Core n)
  | Type (Type Name)
  | Lit Literal
  deriving (Show, Eq)

instance Pretty b => Pretty (Core b) where
  pretty (Lambda binder exp) = nest 2 $ pretty "\\" <> pretty binder <+> pretty "->" <> softline <> pretty exp
  pretty (App func arg)      = parensIf (needsParens func) (pretty func) <> parens (pretty arg)
    where needsParens (Var _) = False
          needsParens (App _ _) = False
          needsParens _       = True
  pretty (Case scrut alts)   =
    pretty "case" <+> pretty scrut <+> pretty "of" <+>
    braces (nest 2 (hardline <> prettiedAlts) <> hardline)
    where prettiedAlts = vsep' $ map pretty alts
  pretty (Var id)            = pretty id
  pretty (Let binder exp)    = hang 2 $ pretty "let" <+> pretty binder <> softline <>
                               pretty "in" <+> pretty exp
  pretty (Type ty)           = pretty ty
  pretty (Lit lit)           = pretty lit

instance Pretty b => Pretty (Alt b) where
  pretty (ConAlt n binders exp) = pretty n <+> hsep (map pretty binders) <+> pretty "->" <+> pretty exp
  pretty (TrivialAlt exp) = pretty "_" <+> pretty "->" <+> pretty exp

instance Pretty n => Pretty (Bind n) where
  pretty (NonRec nm exp) = pretty nm <+> pretty "=" <+> pretty exp

instance Pretty Var where
  pretty (TyVar{ name = name}) = pretty name
  pretty (Id{ name = name, usage = Used }) = pretty name
  pretty (Id{ name = name, usage = NotUsed }) = pretty "_"

data Var
  = TyVar { name :: Id, kind :: Kind }
  | Id { name :: Id, ty :: Type Name, usage :: Usage }
  deriving (Show, Eq)

data Usage
  = NotUsed
  | Used
  deriving (Show, Eq)

data Alt b
  = ConAlt Id [b] (Core b)
  | TrivialAlt (Core b)
  deriving (Show, Eq)

type Arg n = Core n
type CoreExp = Core Var

data Bind n
  = NonRec n (Core n)
  deriving (Show, Eq)

