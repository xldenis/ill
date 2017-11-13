module Ill.Desugar
( module Ill.Desugar
, module X
) where

import Ill.Syntax hiding (Expression(..), ty)
import Ill.Syntax.Core

import qualified Ill.Syntax as S

import Ill.Desugar.Trait as X
import Ill.Desugar.Cases as X
import Ill.Desugar.BinOp as X

import Control.Monad.State
import Debug.Trace
type Id = Name

{-
  Syntax definition for core lambda calculus representation

  At this point all top level bindings should have cases pushed in,
  traits should have been desugared.

  Data constructors are still kept as implicit bindings
  to variables with the name of the contrsuctor.
-}

data CoreModule = Mod
  { bindings :: [Bind Var]
  , constructors :: [(Name, Int)] -- wip: more generally track constructor info
  } deriving (Show, Eq)


declsToCore :: [Decl TypedAnn] -> CoreModule
declsToCore decls = execState (mapM declToCore' decls) (Mod [] [])

declToCore' :: Decl TypedAnn -> State CoreModule ()
declToCore' (a :< Value nm [([], exp)]) = do
  -- traceShowM (fromTyAnn a)
  let vars   = unforall (fromTyAnn a)
      tBinds = map (\var -> TyVar var Star) vars
      bindExp = foldl (\core binder -> Lambda binder core) (toCore exp) tBinds

  modify $ \m -> m { bindings = NonRec binder bindExp : bindings m }
  where
  unforall (Forall tVars _) = tVars
  unforall _                = []

  binder = Id { varName = nm, ty = fromTyAnn a, usage = Used }
declToCore' (_ :< Data nm _ conses) = do
  let cons' = map (\cons ->
        case unwrapProduct cons of
          (TConstructor consNm : args) -> (consNm, length args)
        ) conses
  modify $ \m -> m { constructors = cons' ++ constructors m }

declToCore' (_) = pure ()


toCore :: Expr TypedAnn -> CoreExp
toCore (_ :< S.Var n) = Var n
toCore (_ :< S.Constructor n) = Var n
toCore (_ :< S.Case scrut alts) = Case (toCore scrut) (toAlts alts)
toCore (_ :< S.Assign names exprs) = error "assignments must be desugared in blocks"
toCore (_ :< S.Apply lam args) = foldl App (toCore lam) (map toCore args)
toCore (_ :< S.BinOp op left right) = error "binops should have been desugared to assigns"

toCore (_ :< S.If cond left right) = Case (toCore cond)
  [ ConAlt "True" [] (toCore left)
  , ConAlt "False" [] (toCore right)
  ]
toCore (_ :< S.Lambda bind exp) = let
  vars = map toVar bind
  in foldr Lambda (toCore exp) vars
  where
  toVar (a :< S.PVar nm) = Id { varName = nm, ty = fromTyAnn a, usage = Used }
  isVarPat (_ :< S.PVar _) = True
  isVarPat _ = False
toCore (_ :< S.Body exps) = toCore' exps
  where
  toCore' :: [Expr TypedAnn] -> CoreExp
  toCore' ((_ :< a@(S.Assign _ _ )): []) = error (show a) -- i think? i dont like this :(
  toCore' ((_ :< S.Assign names exprs) : others) = let
    binders = map toBinder $ zip names exprs
    toBinder (nm, expr) = NonRec (Id nm (typeOf expr) Used) (toCore expr)
    in foldr Let (toCore' others) binders
  toCore' [e] = toCore e
  toCore' (e:others) = toCore e `mkSeq` (toCore' others)
    where
    mkSeq a b = (App (App (Var "seq") a) b)
toCore (_ :< S.Literal lit ) = Lit lit

toAlts = map toAlt

toAlt (a :< Wildcard, e) = TrivialAlt $ toCore e
toAlt (_ :< PLit lit, exp) = LitAlt lit $ toCore exp
toAlt (_ :< Destructor n ps, exp) = ConAlt n (map toVar ps) $ toCore exp
  where
  toVar (a :< Wildcard) = Id "" (fromTyAnn a) NotUsed
  toVar (a :< PVar nm)  = Id nm (fromTyAnn a) Used
  toVar a = error (show a)
