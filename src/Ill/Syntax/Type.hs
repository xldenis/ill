{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances  #-}
module Ill.Syntax.Type
( Type(..)
, ty2sTy
) where

import Ill.Syntax.Pretty

import qualified Ill.Inference.Type as T

data Type t
  = TVar t
  | TAp (Type t) (Type t)
  | Constructor t
  | Arrow (Type t) (Type t)
  | Trait t (Type t)
  | Constraint [Type t] (Type t)
  deriving (Eq, Show)

instance Pretty (Type String) where
  pretty (TVar var) = pretty var
  pretty (TAp f a) = parens $ go f <+> pretty a
    where go (TAp f' a') = go f' <+> pretty a
          go a           = pretty a
  pretty (Constructor cons) = text cons
  pretty (Arrow from to) = parensIf (complex from) (pretty from) <+> text "->" <+> parensIf (complex to) (pretty to)
  pretty (Trait nm tp) = text nm <+> pretty tp
  pretty (Constraint trts tp) = alternative (map pretty trts) <+> pretty tp
    where alternative = encloseSep empty (empty <+> char '|') (char ',')

complex :: Type t -> Bool
complex (Constructor _) = False
complex (Arrow _ _) = True
complex _ = False

ty2sTy :: T.Type -> Type T.Id
ty2sTy (T.TVar (T.Tyvar i _)) = TVar i
ty2sTy (T.TCon (T.Tycon i _)) = Constructor i
ty2sTy (T.TAp f e) = case ty2sTy f of
  TAp (Constructor "(->)") a -> Arrow a (ty2sTy e)
  f' -> TAp f' (ty2sTy e)
