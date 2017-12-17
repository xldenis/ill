{-# LANGUAGE DeriveDataTypeable #-}
module Ill.Syntax.Literal where

import Ill.Prelude

import Ill.Syntax.Pretty
import Data.Data

import Ill.Syntax.Type

data Literal
  = RawString String
  | EscString String
  | Integer Integer
  | Double Double
  deriving (Eq, Show, Ord, Data)

instance Pretty Literal where
  pretty (RawString x) = squotes (pretty x)
  pretty (EscString x) = dquotes (pretty x)
  pretty (Integer x) = pretty x
  pretty (Double x) = pretty x

litType (RawString _) = tString
litType (EscString _) = tString
litType (Integer _ )  = tInteger
litType (Double _)    = tDouble
