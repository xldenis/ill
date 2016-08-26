{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances  #-}
module Ill.Syntax.Type
( Type(..)
) where

  import Ill.Syntax.Pretty

  data Type t
    = TVar t
    | Constructor t [Type t]
    | Arrow (Type t) (Type t)
    | Trait t (Type t)
    | Constraint [Type t] (Type t)
    deriving (Show)

  instance Pretty (Type String) where
    pretty (TVar var) = pretty var
    pretty (Constructor cons args) = text cons <+> cat (punctuate comma (map (\a -> parensIf (complex a) $ pretty  a) args))
    pretty (Arrow from to) = parensIf (complex from) (pretty from) <+> text "->" <+> parensIf (complex to) (pretty to)
    pretty (Trait nm tp) = text nm <+> pretty tp
    pretty (Constraint trts tp) = alternative (map pretty trts) <+> pretty tp
      where alternative = encloseSep empty (empty <+> char '|') (char ',')

  complex :: Type t -> Bool
  complex (Constructor _ _) = True
  complex (Arrow _ _) = True
  complex _ = False

