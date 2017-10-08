module Ill.Desugar where

import Ill.Syntax hiding (Expression(..), ty)
import qualified Ill.Syntax as S

type Id = Name

{-
  Syntax definition for core lambda calculus representation

  At this point all top level bindings should have cases pushed in,
  traits should have been desugared.

  Data constructors are still kept as implicit bindings
  to variables with the name of the contrsuctor.
-}

data Core n
  = Lambda n (Core n)
  | App (Core n) (Arg n)
  | Case (Core n) [Alt n]
  | Var Id
  | Let (Bind n) (Core n)
  | Type (Type Name)
  | Lit Literal
  deriving (Show)

data Var
  = TyVar { name :: Id, kind :: Kind }
  | Id { name :: Id, ty :: Type Name }
  deriving (Show)

data Alt b
  = ConAlt [b] (Core b)
  | TrivialAlt (Core b)
  deriving Show

type Arg n = Core n
type CoreExp = Core Var

data Bind n
  = NonRec n (Core n)
  deriving (Show)
  -- | Rec

toCore :: Expr TypedAnn -> CoreExp
toCore (_ :< S.Var n) = Var n
toCore (_ :< S.Case scrut alts) = Case (toCore scrut) (toAlts alts)
toCore (_ :< S.Assign names exprs) = error "assignments must be desugared in blocks"
toCore (_ :< S.Apply lam args) = foldl App (toCore lam) (map toCore args)
toCore (_ :< S.Lambda bind exp) = let
  vars = map toVar bind
  in foldr Lambda (toCore exp) vars
  where
  toVar (a :< S.PVar nm) = Id { name = nm, ty = fromTyAnn a }
  isVarPat (_ :< S.PVar _) = True
  isVarPat _ = False
toCore (_ :< S.Body exps) = toCore' exps
  where
  toCore' :: [Expr TypedAnn] -> CoreExp
  toCore' ((_ :< S.Assign _ _ ): []) = error "omg" -- i think? i dont like this :(
  toCore' ((_ :< S.Assign names exprs) : others) = let
    binders = map toBinder $ zip names exprs
    toBinder (nm, expr) = NonRec (Id nm $ typeOf expr) (toCore expr)
    in foldr Let (toCore' others) binders
  toCore' [e] = toCore e
  toCore' (e:others) = toCore e `mkSeq` (toCore' others)
    where
    mkSeq a b = (App (App (Var "seq") a) b)

toAlts = map toAlt

toAlt (a :< Wildcard, e) = TrivialAlt $ toCore e
-- toAlt (_ :< Destructor ps, exp) = ConAlt toCore exp
