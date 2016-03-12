{-# LANGUAGE DeriveFunctor, DeriveTraversable, DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}

module Ill.Syntax where

  import Control.Comonad.Cofree

  type Prefix = String

  type Qualified = Bool

  type Name = String

  type Alias = Maybe String

  data Module a = Module Name [Decl a] deriving (Show)

  data Declaration a b
    = DataDeclaration Name [Type]
    | TypeSynonymDeclaration Type Type
    | ValueDeclaration Name (Maybe Type) [Pattern] [Expr a]
    | ImportDeclaration Qualified Masks String Alias
    deriving (Functor, Show)

  type Decl a = Cofree (Declaration a) a

  data Masks
    = Hiding [Name]
    | Only  [Name]
    | All
    deriving (Show)

  data Type
    = TVar String
    | Constructor String [Type]
    | Name String
    deriving (Show)

  data Expression a
    = Apply a [a]
    | Assign [Name] [a]
    | Case a [(Pattern, a)]
    | If a a a
    | Lambda [Pattern] a
    | Var Name
    | Literal Literal
    | Body [a]
    deriving (Functor, Show)

  data Literal
    = RawString String
    | EscString String
    | Integer Integer
    | Double Double
    deriving (Show)

  type Expr a = Cofree Expression a

  data Pattern
    = Destructor String [Pattern]
    | Wildcard
    | PVar Name
    | Nil
    deriving (Show)
