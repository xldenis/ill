{-# LANGUAGE DeriveFunctor, DeriveTraversable, DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}

module Ill.Syntax where

  import Control.Comonad.Cofree
  --import Bound (Scope)

  data Module = Module [Declaration]

  data Declaration
    = DataDeclaration Name [Type]
    | TypeSynonymDeclaration Type Type
    -- | ValueDeclaration [Pattern] [Expression () ()]
    | ImportDeclaration Qualified Masks Prefix String Alias

  type Prefix = String

  type Qualified = Bool

  data Masks
    = Hiding [Name]
    | Only  [Name]
    | All

  type Alias = String

  data Type
    = TVar String
    | Constructor String [Type]
    | Name String

  type Name = String

  data Expression a
    = Apply a [a]
    | Assign [Name] [a]
    | Case a [(Pattern, a)]
    | If a a a
    | Lambda [Pattern] a
    | Var Name
    | Literal Literal

  data Literal
    = RawString String
    | EscString String
    | Integer Integer
    | Double Double

  type Expr a = Cofree Expression a

  data Pattern
    = Destructor String [Pattern]
    | Wildcard
    | PVar Name
    | Nil
