{-# LANGUAGE DeriveFunctor, DeriveAnyClass, DeriveTraversable, DeriveFoldable, DeriveDataTypeable #-}
module Ill.Syntax.Core where

import           Ill.Prelude

import Control.Lens.Extras (is)
import Control.Lens.Plated

import Data.Data

import Ill.Syntax.Kind
import Ill.Syntax.Literal
import Ill.Syntax.Name
import Ill.Syntax.Pretty
import Ill.Syntax.Type

-- Look into using recursion schemes to simplify tons of codebase
data Core n
  = Lambda n (Core n)
  | App (Core n) (Arg n)
  | Case (Core n) [Alt n]
  | Var Var
  | Let (Bind n) (Core n)
  | Type (Type Name)
  | Lit Literal
  deriving (Show, Eq, Functor, Applicative, Foldable, Traversable, Data)

instance Data a => Plated (Core a)

-- These are convenience classes for when a specific constraint is wanted
-- TODO: either expand their use or scrap them altogether
class HasName n where
  name :: n -> Id

class HasType n where
  getTy :: n -> Type Name

instance HasType Var where
  getTy = idTy

instance HasName Var where
  name = varName

data Var
  = TyVar { varName :: Id, kind :: Kind }
  | Id { varName :: Id, idTy :: Type Name, usage :: Usage }
  deriving (Show, Eq, Data)

data Usage
  = NotUsed
  | Used
  deriving (Show, Eq, Data)

data Alt b
  = ConAlt Id [b] (Core b)
  | TrivialAlt (Core b)
  | LitAlt Literal (Core b)
  deriving (Show, Eq, Functor, Applicative, Foldable, Traversable, Data)

isTrivialAlt (TrivialAlt _) = True
isTrivialAlt _ = False

isLitAlt (LitAlt _ _) = True
isLitAlt _ = False

isConAlt (ConAlt{}) = True
isConAlt _ = False

type Arg n = Core n
type CoreExp = Core Var

data Bind n
  = NonRec n (Core n)
  deriving (Show, Eq, Functor, Applicative, Foldable, Traversable, Data)

data CoreModule = Mod
  { bindings :: [Bind Var]
  , constructors :: [(Name, (Int, Type Name))] -- wip: more generally track constructor info
  , primitives :: [Id]
  } deriving (Show, Eq)

emptyModule = Mod [] [] ["failedPattern", "plusInt", "minusInt", "multInt", "divInt", "eqInt", "ltInt", "gtInt", "leqInt", "geqInt", "maxInt", "minInt", "plusStr", "showInt"]

substitute :: HasName n => (Id, Core n) -> Core n -> Core n
substitute = go []
  where
  -- Are binders recursive ie: let a = a in a ?
  -- go :: [Id] -> (Id, CoreExp) -> CoreExp -> CoreExp
  go bound (nm, exp) (Var id)
    | nm == varName id && not (nm `elem` bound) = exp

  go bound subst     (Lambda n exp)     = Lambda n (go (name n : bound) subst exp)
  go bound subst     (App exp arg)      = App (go bound subst exp) (go bound subst arg)
  go bound subst     (Case scrut alts)  = Case (go bound subst scrut) (map goAlt alts)
    where
    goAlt (TrivialAlt exp)   = TrivialAlt (go bound subst exp)
    goAlt (ConAlt id bs exp) = ConAlt id bs (go (map name bs ++ bound) subst exp)
    goAlt (LitAlt lit exp)   = LitAlt lit (go bound subst exp)
  go bound subst     (Let bind exp)     = Let (subBinder bind) (go (boundNames bind : bound) subst exp)
    where
    boundNames (NonRec n _)  = name n
    subBinder (NonRec n exp) = NonRec n (go bound subst exp)
  go _     _         x                  = x

{-
  Calculates the type of a core term. Assumes the core term is valid.
-}
getTyOf :: CoreExp -> Type Name
getTyOf b@(App f a) = getTyOf a `applyArgumentToType` getTyOf f
getTyOf (Case _ alts) = altTyOf $ head alts
  where altTyOf (TrivialAlt e) = getTyOf e
        altTyOf (LitAlt _ e)   = getTyOf e
        altTyOf (ConAlt _ _ e) = getTyOf e
getTyOf (Lambda b@(Id{}) exp) = (idTy b) `tFn` getTyOf exp
getTyOf (Lambda b@(TyVar{}) exp) = generalizeWith [varName b] $ getTyOf exp
getTyOf (Let _ e) = getTyOf e
getTyOf (Var v) = idTy v
getTyOf (Type t) = t
getTyOf (Lit l) = litType l
getTyOf v = error $ show v

instance (HasType b, Pretty b) => Pretty (Core b) where
  pretty (Lambda binder exp) = nest 2 $ pretty "\\" <> pretty binder <+> pretty "->" <> softline <> pretty exp
  pretty a@(App _ _)    = nest 2 $ parensIf (needsParens f) (pretty f) <> softline' <> (argify (map prettyAp args))
    where needsParens (Var _) = False
          needsParens (App _ _) = False
          needsParens _       = True
          prettyAp (Type ty) = pretty "@" <+> pretty ty
          prettyAp app       = pretty app
          unwrapApp (App f a) acc = unwrapApp f (a : acc)
          unwrapApp (f) acc = f : acc
          argify = tupled -- parens . concatWith (surround $ pretty ", ")
          (f : args) = unwrapApp a []
  pretty (Case scrut alts)   =
    pretty "case" <+> pretty scrut <+> pretty "of" <+>
    braces (nest 2 (hardline <> prettiedAlts) <> hardline)
    where prettiedAlts = vsep' $ map pretty alts
  pretty (Var id)            = pretty id
  pretty l@(Let _ _)    = group . align $ pretty "let" <+> (align $ concatWith (\x y -> x <> hardline <> y) (map pretty binds)) <> line <>
                          pretty "in" <+> pretty inner
    where go (Let b exp) = fmap (b :) $ go exp
          go a = (a, [])
          (inner, binds) = go l

  pretty (Type ty)           = pretty ty
  pretty (Lit lit)           = pretty lit

instance (HasType b, Pretty b) => Pretty (Alt b) where
  pretty (ConAlt n binders exp) = pretty n <+> hsep (map pretty binders) <+> pretty "->" <> softline <> pretty exp
  pretty (TrivialAlt exp) = pretty "_" <+> pretty "->" <> softline <> pretty exp
  pretty (LitAlt lit exp) = pretty lit <+> pretty "->" <> softline <> pretty exp
instance (HasType n, Pretty n) => Pretty (Bind n) where
  pretty (NonRec nm exp) = parens (pretty nm <+> pretty "::" <+> pretty (getTy nm)) <+> pretty "=" <+> pretty exp

instance Pretty Var where
  pretty (TyVar{ varName = name}) = parens $ pretty "@" <> pretty name
  pretty (Id{ varName = name, usage = Used, idTy = ty }) = pretty name
  pretty (Id{ varName = name, usage = NotUsed }) = pretty "_"
