{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances  #-}
module Ill.Syntax.Type
( Type(..)
, TypeF(..)
, Fix(..)
) where

  import Ill.Syntax.Pretty

  data TypeF t a
    = TVar t
    | Constructor t [a]
    | Arrow a a
    | Trait t a
    | Constraint [a] a
    deriving (Show)

  type Type = Fix (TypeF String)

  newtype Fix f = Fix { unfix :: f (Fix f)}

  instance Pretty (f (Fix f)) => Pretty (Fix f) where
    pretty = pretty . unfix

  instance Show (f (Fix f)) => Show (Fix f) where
    show x = "(" ++ show (unfix x) ++ ")"

  instance Pretty (TypeF String (Fix (TypeF String))) where
    pretty (TVar var) = pretty var
    pretty (Constructor cons args) = text cons <+> (cat $ punctuate comma (map (\a -> parensIf (complex a) $ pretty  a) args))
    pretty (Arrow from to) = parensIf (complex from) (pretty from) <+> (text "->") <+> parensIf (complex to) (pretty to)
    pretty (Trait nm tp) = text nm <+> (pretty tp)
    pretty (Constraint trts tp) = (alternative $ map pretty trts) <+> pretty tp
      where alternative = encloseSep empty (empty <+> char '|') (char ',')

  complex :: Type -> Bool
  complex (Fix (Constructor _ _)) = True
  complex (Fix (Arrow _ _)) = True
  complex _ = False

