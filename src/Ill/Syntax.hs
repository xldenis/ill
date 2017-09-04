{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Ill.Syntax
( module X
, module Ill.Syntax
, Cofree(..)
) where
import           Control.Comonad.Cofree
import           Control.Lens.TH

import           Ill.Syntax.Expression as X
import           Ill.Syntax.Literal as X
import           Ill.Syntax.Pattern as X
import           Ill.Syntax.Type as X
import           Ill.Syntax.Kind as X

import           Control.Comonad        (extend)
import           Control.Lens           (each, over, _2)
import           Ill.Syntax.Pretty

import           Text.Megaparsec (SourcePos)

import           Data.List (intersperse)

type Prefix = String

type Qualified = Bool

type Name = String

type Alias = Maybe String

data Module a = Module Name [Decl a] deriving (Eq, Show)

data Declaration a b
  = Data Name [Name] [Type Name]
  | TypeSynonym Name [Name] (Type Name)
  | Value Name [(Patterns, Expr a)]
  | Signature Name (Type Name)
  | Import Qualified Masks String Alias
  | TraitDecl [Constraint Name] Name [Name] [b]
  | TraitImpl [Constraint Name] Name [Type Name] [b]
  deriving (Eq, Functor, Show)

type Decl a = Cofree (Declaration a) a

data SourceSpan = SourceSpan {begin :: SourcePos, end :: SourcePos} deriving (Eq, Show)

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

nestedFmap :: (a -> b) -> Decl a -> Decl b
nestedFmap f v = hoistCofree (go f) $ fmap f v
  where
  go f (Value n es) = Value n $ over (each . _2) (fmap f) es
  go f (Data a v b) = Data a v b
  go f (TypeSynonym a vs b) = TypeSynonym a vs b
  go f (Signature a b) = Signature a b
  go f (Import q m s a) =  Import q m s a
  go f (TraitDecl a b c d) = TraitDecl a b c d
  go f (TraitImpl a b c d) = TraitImpl a b c d

dropAnn :: Decl a -> Decl ()
dropAnn = nestedFmap (const ())

data Masks
  = Hiding [Name]
  | Only  [Name]
  | All
  deriving (Eq, Show)

makePrisms ''Declaration

instance Pretty (Module a) where
  pretty (Module name decls) = nest 2 (pretty "module" <+> pretty name `above`
    vsep (intersperse mempty (map pretty decls))) `above`
    pretty "end"

instance Pretty (Cofree (Declaration a) a) where
  pretty (_ :< Data name vars cons) = pretty "data" <+> pretty name <+> hsep (map pretty vars) <+> pretty '=' <+> alternative (map pretty cons)
    where alternative = encloseSep mempty mempty (mempty <+> pretty '|')
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
    constraints c = if null c then mempty else hsep (punctuate comma (map pretty c)) <+> pretty "|"
    declarationLine = pretty "impl" <+> constraints supers <+> pretty trtNm <+> (hsep $ map pretty args)