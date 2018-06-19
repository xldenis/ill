{-# LANGUAGE DeriveDataTypeable #-}
module Ill.Syntax.Literal where

import Ill.Prelude

import Ill.Syntax.Pretty
import Data.Data

import Ill.Syntax.Type
import Ill.Syntax.Name

data Literal
  = RawString String
  | Integer Integer
  | Double Double
  | Char Char
  deriving (Eq, Show, Ord, Data)

instance Pretty Literal where
  pretty (RawString x) = dquotes (pretty x)
  pretty (Integer x) = pretty x
  pretty (Double x) = pretty x
  pretty (Char x) = squotes (pretty x)

litType :: Literal -> Type QualifiedName
litType (RawString _) = tString
litType (Integer _ )  = tInteger
litType (Double _)    = tDouble
litType (Char _)      = tChar
