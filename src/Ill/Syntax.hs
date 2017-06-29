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

import Data.List (intersperse)

type Prefix = String

type Qualified = Bool

type Name = String

type Alias = Maybe String

data Module a = Module Name [Decl a] deriving (Eq, Show)

data Declaration a b
  = Data Name [Name] [Type Name]
  | TypeSynonym Name [Name] (Type Name)
  | Value Name [([Pattern], Expr a)]
  | Signature Name (Type Name)
  | Import Qualified Masks String Alias
  | TraitDecl (Type Name) [b]
  | TraitImpl (Type Name) [b]
  deriving (Eq, Functor, Show)

type Decl a = Cofree (Declaration a) a

isValue :: Decl a -> Bool
isValue (_ :< Value _ _) = True
isValue _ = False

isDataDecl :: Decl a -> Bool
isDataDecl (_ :< Data _ _ _) = True
isDataDecl _ = False

nestedFmap :: (a -> b) -> Decl a -> Decl b
nestedFmap f v = hoistCofree (go f) $ fmap f v
  where
  go f (Value n es) = Value n $ over (each . _2) (fmap f) es
  go f (Data a v b) = Data a v b
  go f (TypeSynonym a vs b) = TypeSynonym a vs b
  go f (Signature a b) = Signature a b
  go f (Import q m s a) =  Import q m s a
  go f (TraitDecl a b) = TraitDecl a b
  go f (TraitImpl a b) = TraitImpl a b

dropAnn :: Decl a -> Decl ()
dropAnn = nestedFmap (const ())

data Masks
  = Hiding [Name]
  | Only  [Name]
  | All
  deriving (Eq, Show)

makePrisms ''Declaration

instance Pretty (Module a) where
  pretty (Module name decls) = nest 2 (text "module" <+> text name `aboveBreak`
    vsep (intersperse empty (map pretty decls))) `aboveBreak`
    text "end"

instance Pretty (Cofree (Declaration a) a) where
  pretty (_ :< Data name vars cons) = text "data" <+> text name <+> hsep (map pretty vars) <+> char '=' <+> alternative (map pretty cons)
    where alternative = encloseSep empty empty (empty <+> char '|')
  pretty (_ :< TypeSynonym alias vars target) = text "type" <+> pretty alias <+> pretty vars <+> text "=" <+> pretty target
  pretty (_ :< Value name cases) = vsep (headBranch : map otherBranch (tail cases)) `aboveBreak` text "end"
    where branch (args, body) = nest 2 $ tupled (map pretty args) `aboveBreak` pretty body
          headBranch    = text "fn" <+> text name <+> branch (head cases)
          otherBranch b = text "or" <+> text name <+> branch b
  pretty (_ :< Signature func tp) = text func <+> text "::" <+> pretty tp
  pretty (_ :< Import qual msk name alias) = text "import" <-> when (const $ text "qualified") qual empty
    <-> text name <-> prettyJust alias <-> prettyMask msk
      where prettyJust (Just alias') = text "as" <+> text alias'
            prettyJust  Nothing     = empty
            prettyMask (Hiding nms) = text "hiding" <+> tupled (map pretty nms)
            prettyMask (Only   nms) = tupled $ map pretty nms
            prettyMask _            = empty
  pretty (_ :< TraitDecl trt body)  = nest 2 (text "trait" <+> pretty trt `above` vsep (map pretty body)) `above` text "end"
    -- where constraints c = if null c then empty else hsep (punctuate comma (map pretty c)) <+> text "|"
  pretty (_ :< TraitImpl trt body)  = nest 2 (text "impl" <+> pretty trt `above` vsep (map pretty body)) `above` text "end"

