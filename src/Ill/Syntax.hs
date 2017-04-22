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
) where
import           Control.Comonad.Cofree
import           Control.Lens.TH

import           Ill.Syntax.Expression as X
import           Ill.Syntax.Literal as X
import           Ill.Syntax.Pattern as X
import           Ill.Syntax.Type as X

import           Ill.Inference.Class

import           Control.Comonad        (extend)
import           Control.Lens           (each, over, _2)
import           Ill.Syntax.Pretty

type Prefix = String

type Qualified = Bool

type Name = String

type Alias = Maybe String

data Module a = Module Name [Decl a] deriving (Eq, Show)

data Declaration a b
  = Data Name [Type Name]
  | TypeSynonym (Type Name) (Type Name)
  | Value Name [([Pattern], [Expr a])]
  | Signature Name (Type Name)
  | Import Qualified Masks String Alias
  | TraitDecl (Type Name) [b]
  | TraitImpl (Type Name) [b]
  deriving (Eq, Functor, Show)

type Decl a = Cofree (Declaration a) a

fuckComonads :: Declaration a b -> Declaration () b
fuckComonads (Value n es) = Value n $ over (each . _2 . each ) (extend $ const ()) es
fuckComonads (Data a b) = Data a b
fuckComonads (TypeSynonym a b) = TypeSynonym a b
fuckComonads (Signature a b) = Signature a b
fuckComonads (Import q m s a) =  Import q m s a
fuckComonads (TraitDecl a b) = TraitDecl a b
fuckComonads (TraitImpl a b) = TraitImpl a b

dropAnn :: Decl a -> Decl ()
dropAnn = hoistCofree (fuckComonads) . (extend $ const ())

data Masks
  = Hiding [Name]
  | Only  [Name]
  | All
  deriving (Eq, Show)

makePrisms ''Declaration

instance Pretty (Module a) where
  pretty (Module name decls) = nest 2 (text "module" <+> text name `aboveBreak` vsep (map pretty decls)) `aboveBreak` text "end"

instance Pretty (Cofree (Declaration a) a) where
  pretty (_ :< Data name sum) = text "data" <+> text name <+> char '=' <+> alternative (map pretty sum)
    where alternative = encloseSep empty empty (char '|')
  pretty (_ :< TypeSynonym alias target) = text "type" <+> pretty alias <+> text "=" <+> pretty target
  pretty (_ :< Value name cases) = text "fn" <+> text name <+> branch (head cases) `aboveBreak` vsep (map (\c -> text "or" <+> text name <+> branch c) $ tail cases) `aboveBreak` text "end"
    where branch (args, body) = nest 2 $ tupled (map pretty args) `aboveBreak` vsep (map pretty body)
  pretty (_ :< Signature func tp) = text func <+> text "::" <+> pretty tp
  pretty (_ :< Import qual msk name alias) = text "import" <-> when (const $ text "qualified") qual empty
    <-> text name <-> prettyJust alias <-> prettyMask msk
      where prettyJust (Just alias) = text "as" <+> text alias
            prettyJust  Nothing     = empty
            prettyMask (Hiding nms) = text "hiding" <+> tupled (map pretty nms)
            prettyMask (Only   nms) = tupled $ map pretty nms
            prettyMask _            = empty
  pretty (_ :< TraitDecl trt body) = nest 2 (text "trait" <+> pretty trt `above` vsep (map pretty body)) `above` text "end"
    where constraints c = if null c then empty else hsep (punctuate comma (map pretty c)) <+> text "|"


-- tiAlt :: Infer ([Pattern], Expr a) Type
-- tiAlt ce as (pats, body) = do
--   (pPred, pAssum, pTy) <- tiPats pats
--   (ePred, tBody) <- tiExpr ce (as ++ pAssum) body

--   let fTy = foldr1 fn pTy
--   return (pPred ++ ePred, fTy `fn` tBody)

-- tiAlts :: Infer [([Pattern], Expr)] Type
-- tiAlts ce as alts = do
--   pInf <- mapM (tiAlt ce as) alts
