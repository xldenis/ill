{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Thrill.Desugar.Administrative where

{-
  Administrative Normal Form

  Administrative Normal Form or A-Normal Form or ANF is a restricted subset of the lambda calculus.
  It restricts the expressions which can be arguments to functions, by requiring them to be 'atomic'.

  Those atomic terms consist of:

  - Variables
  - Literals
  - Type applications
  - Type lambdas
  Non atomic terms (eg: applications) must be pulled out and bound in a let-expression.

  For example:

  f(g(1), 2)

  becomes:

  let anf0 = g(1)
  in f(anf0, 2)

  The purpose of ANF is to help simplify optimizations and generate better code at the end.
  Several papers have shown that most of the advantages of CPS can be obtained through ANF,
  while remaining simpler to understand, implement and work with.

  Unclear Points

  - Are type applications still atomic
    - hunch says yes
-}

import           Thrill.Prelude

import           Control.Monad.Fresh
import           Control.Monad.Reader
import           Control.Monad.Identity
import           Thrill.Syntax.Core

import           Control.Lens.Plated
import           Thrill.Syntax.Literal
import           Thrill.Syntax.Name
import           Thrill.Syntax.Pretty
import           Thrill.Syntax.Type

normalize :: CoreModule -> CoreModule
normalize m@(Mod{..}) =
  m { bindings = (evalFresh 0 . flip runReaderT coreModuleName $ forM bindings bindToANF) }
  where bindToANF (NonRec x e) = NonRec x <$> (transformM toANF e)

isAtom (Var v)      = True
isAtom (Lit l)      = True
isAtom (Type t)     = True
isAtom (Case _ _)   = False
isAtom (Lambda TyVar{} _) = True
isAtom (App a (Type _)) | isAtom a = True
isAtom _            = False

toANF :: CoreExp -> ReaderT Name (FreshT Identity) CoreExp
toANF l@(Lambda bind exp) = pure l
toANF (App f (Let b exp)) = Let b <$> (toANF $ App f exp)
toANF (App (Let b f) a) = Let b <$> (toANF $ App f a)
toANF a@(App f arg) = case isAtom arg of
  True -> pure a
  False -> do
    nm <- Qualified <$> ask <*> prefixedName "anf"
    f' <- toANF f
    arg' <- toANF arg
    let ty = getTyOf arg'
    let v = Id nm ty Used

    pure $ Let (NonRec v arg') (App f' (Var v)) -- need to generate fresh names!
toANF c@(Case scrut alts) = pure c
toANF v@(Var id)    = pure v
toANF l@(Let b exp) = pure l
toANF t@(Type _)    = pure t
toANF l@(Lit  _)    = pure l
