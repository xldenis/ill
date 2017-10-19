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

substitute :: HasName n => (Id, Core n) -> Core n -> Core n
substitute = go []
  where
  -- Are binders recursive ie: let a = a in a ?
  -- go :: [Id] -> (Id, CoreExp) -> CoreExp -> CoreExp
  go bound (nm, exp) (Var id)
    | nm == id && not (nm `elem` bound) = exp

  go bound subst     (Lambda n exp)     = Lambda n (go (name n : bound) subst exp)
  go bound subst     (App exp arg)      = App (go bound subst exp) (go bound subst arg)
  go bound subst     (Case scrut alts)  = Case (go bound subst scrut) (map goAlt alts)
    where
    goAlt (TrivialAlt exp)   = TrivialAlt (go bound subst exp)
    goAlt (ConAlt id bs exp) = ConAlt id bs (go (map name bs ++ bound) subst exp)
  go bound subst     (Let bind exp)     = Let (subBinder bind) (go (boundNames bind : bound) subst exp)
    where
    boundNames (NonRec n _)  = name n
    subBinder (NonRec n exp) = NonRec n (go bound subst exp)
  go _     _         x                  = x

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
  pretty (TyVar{ varName = name}) = pretty name
  pretty (Id{ varName = name, usage = Used }) = pretty name
  pretty (Id{ varName = name, usage = NotUsed }) = pretty "_"

class HasName n where
  name :: n -> Id

instance HasName Var where
  name = varName

data Var
  = TyVar { varName :: Id, kind :: Kind }
  | Id { varName :: Id, ty :: Type Name, usage :: Usage }
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

