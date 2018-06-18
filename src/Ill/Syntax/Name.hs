{-# LANGUAGE DeriveDataTypeable, DeriveFunctor #-}
module Ill.Syntax.Name where

import Ill.Prelude
import Ill.Syntax.Pretty
import Data.String
import Data.Data

type Name = String
type Id = String

data Qualified a
  = Qualified { qualModule :: String, qualName :: a }
  | Internal { qualName :: a }
  deriving (Show, Eq, Ord, Data, Functor)

instance Pretty QualifiedName where
  pretty (Qualified mod a) = pretty mod <> pretty '.' <> pretty a
  pretty (Internal nm) = pretty nm

type QualifiedName = Qualified Name

appendToName (Qualified mod nm) nm2 = Qualified mod (nm ++ nm2)

mergeNames (Qualified mod nm) (Qualified _ nm2) = Qualified mod (nm ++ nm2)

unQualify (Qualified _ nm) = nm
unQualify (Internal nm) = nm
