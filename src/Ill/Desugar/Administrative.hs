{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Ill.Desugar.Administrative where

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

import           Ill.Prelude

import           Control.Monad.Fresh
import           Control.Monad.Reader
import           Ill.Syntax.Core

import           Control.Lens.Plated
import           Ill.Syntax.Literal
import           Ill.Syntax.Name
import           Ill.Syntax.Pretty
import           Ill.Syntax.Type

normalize :: CoreModule -> CoreModule
normalize m@(Mod{..}) =
  m { bindings = (evalFresh 0 $ forM bindings bindToANF) }
  where bindToANF (NonRec x e) = NonRec x <$> (transformM toANF e)

isAtom (Var v)      = True
isAtom (Lit l)      = True
isAtom (Type t)     = True
isAtom (Case _ _)   = False
isAtom (Lambda TyVar{} _) = True
isAtom (App a (Type _)) | isAtom a = True
isAtom _            = False

toANF :: CoreExp -> Fresh CoreExp
toANF l@(Lambda bind exp) = pure l
toANF (App f (Let b exp)) = Let b <$> (toANF $ App f exp)
toANF (App (Let b f) a) = Let b <$> (toANF $ App f a)
toANF a@(App f arg) = case isAtom arg of
  True -> pure a
  False -> do
    nm <- prefixedName "anf"
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

{-
  Calculates the type of a core term. Assumes the core term is valid.
-}
getTyOf :: CoreExp -> Type Name
getTyOf b@(App f a) = getTyOf a `applyArgumentToType` getTyOf f
getTyOf (Case _ alts) = altTyOf $ head alts
  where altTyOf (TrivialAlt e) = getTyOf e
        altTyOf (LitAlt _ e)   = getTyOf e
        altTyOf (ConAlt _ _ e) = getTyOf e
getTyOf (Lambda b@(Id{}) exp) = (idTy b) `tFn` getTyOf exp
getTyOf (Lambda b@(TyVar{}) exp) = generalizeWith [varName b] $ getTyOf exp
getTyOf (Let _ e) = getTyOf e
getTyOf (Var v) = idTy v
getTyOf (Type t) = t
getTyOf (Lit l) = litType l
getTyOf v = error $ show v
