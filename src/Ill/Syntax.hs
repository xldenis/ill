{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE PatternSynonyms       #-}

module Ill.Syntax
( module X
, module Ill.Syntax
, Cofree(..)
) where

import           Control.Comonad.Cofree
import           Control.Comonad
import           Control.Lens.TH

import           Ill.Syntax.Expression as X
import           Ill.Syntax.Literal as X
import           Ill.Syntax.Pattern as X
import           Ill.Syntax.Type as X
import           Ill.Syntax.Kind as X

import           Control.Lens           (each, over, _2)
import           Ill.Syntax.Pretty

import           Text.Megaparsec (SourcePos)

import           Data.List (intersperse, find)
import           Data.Bifunctor

type Prefix = String

type Qualified = Bool

type Name = String

type Alias = Maybe String

data Module a = Module Name [Decl a] deriving (Eq, Show)

data Declaration a b
  = Data Name [Name] [Type Name]
  | TypeSynonym Name [Name] (Type Name)
  | Value Name [(Patterns a, Expr a)]
  | Signature Name (Type Name)
  | Import Qualified Masks String Alias
  | TraitDecl [Constraint Name] Name [Name] [b]
  | TraitImpl [Constraint Name] Name [Type Name] [b]
  deriving (Eq, Functor, Show)

type Decl a = Cofree (Declaration a) a

data Masks
  = Hiding [Name]
  | Only  [Name]
  | All
  deriving (Eq, Show)

makePrisms ''Declaration

instance Bifunctor Declaration where
  bimap :: forall a b c d. (a -> b) -> (c -> d) -> Declaration a c -> Declaration b d
  bimap l r (Data n nms tys) = Data n nms tys
  bimap l r (TypeSynonym n nms ty) = TypeSynonym n nms ty
  bimap l r (Value n brs) = Value n $ map helper brs
    where helper (pats, expr) = (fmap (fmap l) pats, nestedFmap l expr)
  bimap l r (Signature nm ty) = Signature nm ty
  bimap l r (Import q m s a)  = Import q m s a
  bimap l r (TraitDecl cs n nms bs) = TraitDecl cs n nms (map r bs)
  bimap l r (TraitImpl cs n tys bs) = TraitImpl cs n tys (map r bs)

data SourceSpan = SourceSpan {begin :: SourcePos, end :: SourcePos} deriving (Eq, Show)

data TypedAnn = TyAnn { span :: Maybe SourceSpan, ty :: TypeAnn }
  deriving (Show, Eq)

pattern Ann x y = TyAnn (Just x) (Type y)
pattern SynAnn y = TyAnn Nothing (Type y)

data TypeAnn
  = Type (Type Name)
  | Kind Kind
  | None
  deriving (Show, Eq)

-- Spooky partial function. Use only when invariant holds
fromType :: TypeAnn -> Type Name
fromType (Type t) = t
fromType _ = error "impossible expression has non-type annotation"

typeOf :: Functor f => Cofree f TypedAnn -> (Type Name)
typeOf = fromType . ty . extract
  where fromType (Type t) = t

isValue :: Decl a -> Bool
isValue (_ :< Value _ _) = True
isValue _ = False

isDataDecl :: Decl a -> Bool
isDataDecl (_ :< Data _ _ _) = True
isDataDecl _ = False

isSignature :: Decl a -> Bool
isSignature (_ :< Signature _ _) = True
isSignature _ = False

isImpl :: Decl a -> Bool
isImpl (_ :< TraitImpl _ _ _ _) = True
isImpl _ = False

isDecl :: Decl a -> Bool
isDecl (_ :< TraitDecl _ _ _ _) = True
isDecl _ = False

nestedFmap :: (Bifunctor f, Functor (f a)) => (a -> b) -> Cofree (f a) a -> (Cofree (f b) b)
nestedFmap f v = hoistCofree (first f) $ fmap f v

fmapTy f (Type t) = Type (f t)
fmapTy f t        = t

dropAnn :: (Bifunctor f, Functor (f a)) => Cofree (f a) a -> Cofree (f ()) ()
dropAnn = nestedFmap (const ())

lookupFn n (Module _ ds) = find pred ds
  where pred (_ :< Value name _) = n == name
        pred _                   = False

instance Pretty (Module a) where
  pretty (Module name decls) = nest 2 (pretty "module" <+> pretty name `above`
    vsep (intersperse mempty (map pretty decls))) `above`
    pretty "end"

instance Pretty (Cofree (Declaration a) a) where
  pretty (_ :< Data name vars cons) = pretty "data" <+> pretty name <+> hsep' (map pretty vars) <> pretty '=' <+> alternative (map pretty cons)
    where alternative = encloseSep mempty mempty (pretty " | ")
          hsep' [] = mempty
          hsep' xs = hsep xs <+> emptyDoc
  pretty (_ :< TypeSynonym alias vars target) = pretty "type" <+> pretty alias <+> pretty vars <+> pretty "=" <+> pretty target
  pretty (_ :< Value name cases) = vsep (headBranch : map otherBranch (tail cases)) `above` pretty "end"
    where branch (args, body) = nest 2 $ tupled (map pretty args) `above` pretty body
          headBranch    = pretty "fn" <+> pretty name <+> branch (head cases)
          otherBranch b = pretty "or" <+> pretty name <+> branch b
  pretty (_ :< Signature func tp) = pretty func <+> pretty "::" <+> pretty tp
  pretty (_ :< Import qual msk name alias) = pretty "import" <-> when (const $ pretty "qualified") qual mempty
    <-> pretty name <-> prettyJust alias <-> prettyMask msk
      where prettyJust (Just alias') = pretty "as" <+> pretty alias'
            prettyJust  Nothing     = mempty
            prettyMask (Hiding nms) = pretty "hiding" <+> tupled (map pretty nms)
            prettyMask (Only   nms) = tupled $ map pretty nms
            prettyMask _            = mempty
  pretty (_ :< TraitDecl super name args body)  = nest 2 (pretty "trait" <+> declarationLine `above` vsep (map pretty body)) `above` pretty "end"
    where constraints c = if null c then mempty else hsep (punctuate comma (map pretty c)) <+> pretty "|"
          declarationLine = constraints super <+> pretty name <+> (hsep $ map pretty args)
  pretty (_ :< TraitImpl supers trtNm args body) = nest 2 (declarationLine `above` vsep (map pretty body)) `above` pretty "end"
    where
    constraints c = if null c then mempty else hsep (punctuate comma (map prettyCons c)) <+> pretty "|"
    prettyCons (nm, ts) = pretty nm <+> hsep (map pretty ts)
    declarationLine = pretty "impl" <+> constraints supers <+> pretty trtNm <+> (hsep $ map pretty args)
