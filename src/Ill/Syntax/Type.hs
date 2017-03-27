{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances  #-}
module Ill.Syntax.Type
( Type(..)
, ty2sTy
) where

import Ill.Syntax.Pretty

import qualified Ill.Inference.Type as T

data Type t
  = TVar t
  | Constructor t [Type t]
  | Arrow (Type t) (Type t)
  | Trait t (Type t)
  | Constraint [Type t] (Type t)
  deriving (Eq, Show)

instance Pretty (Type String) where
  pretty (TVar var) = pretty var
  pretty (Constructor cons args) = text cons <+> hcat ((map (\a -> parensIf (complex a) $ pretty  a) args))
  pretty (Arrow from to) = parensIf (complex from) (pretty from) <+> text "->" <+> parensIf (complex to) (pretty to)
  pretty (Trait nm tp) = text nm <+> pretty tp
  pretty (Constraint trts tp) = alternative (map pretty trts) <+> pretty tp
    where alternative = encloseSep empty (empty <+> char '|') (char ',')

complex :: Type t -> Bool
complex (Constructor _ []) = False
complex (Constructor _ _) = True
complex (Arrow _ _) = True
complex _ = False

ty2sTy (T.TVar (T.Tyvar i _)) = TVar i
ty2sTy (T.TCon (T.Tycon i _)) = Constructor i []
ty2sTy (T.TAp f e) = case ty2sTy f of
  TVar i -> Constructor i [ty2sTy e]
  Constructor "(->)" [e'] -> Arrow e' (ty2sTy e)
  Constructor i es -> Constructor i (es ++ [ty2sTy e])



