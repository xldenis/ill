module Thrill.Desugar
( module Thrill.Desugar
, module X
) where

import           Thrill.Prelude

import Thrill.Syntax hiding (Expression(..), ty)
import Thrill.Syntax.Core as C

import qualified Thrill.Syntax as S

import Thrill.Desugar.Trait as X
import Thrill.Desugar.Cases as X
import Thrill.Desugar.BinOp as X
import Thrill.Desugar.Administrative as X
import Thrill.Desugar.LambdaLift as X

import Control.Monad.State
import Control.Category ((>>>))

import Thrill.Infer.Monad (Environment, ConstructorEntry(..))
import Debug.Trace

{-
  At this point all top level bindings should have cases pushed in,
  traits should have been desugared.

  Data constructors are still kept as implicit bindings
  to variables with the name of the contrsuctor.
-}

defaultPipeline :: Environment -> Module QualifiedName TypedAnn -> Module QualifiedName TypedAnn
defaultPipeline env = desugarBinOps >>> desugarTraits env >>> desugarPatterns

compileCore :: Module QualifiedName TypedAnn -> CoreModule
compileCore (Module nm desugared) = declsToCore nm desugared & normalize . liftModule

declsToCore :: Name -> [Decl QualifiedName TypedAnn] -> CoreModule
declsToCore nm decls = execState (mapM declToCore' decls) (emptyModule nm)

declToCore' :: Decl QualifiedName TypedAnn -> State CoreModule ()
declToCore' (a :< Value nm [([], exp)]) = do
  let bindExp = (toCore exp)

  modify $ \m -> m { bindings = NonRec binder bindExp : bindings m }
  where
  unforall (Forall tVars _) = tVars
  unforall _                = []

  binder = Id { varName = nm, idTy = fromTyAnn a, usage = Used }
declToCore' (_ :< Data nm args conses) = do
  let cons' = map (\(tag, cons) ->
        case unwrapProduct cons of
          (TConstructor consNm : args) ->
            (consNm, ConstructorEntry consNm (getConstructorType cons) [] (length args) tag)
        ) (zip [0..] conses)
  modify $ \m -> m { constructors = cons' ++ constructors m, types = nm : types m }
  where
  getConstructorType :: Type QualifiedName -> Type QualifiedName
  getConstructorType ty = let
    (TConstructor tyCons : tys) = unwrapProduct ty
    retTy = foldl TAp (TConstructor nm) (map TVar args)
    in generalize $ foldr tFn retTy tys
declToCore' (_) = pure ()

{-
  Get the type applications needed for a type annotation
-}
getTypeApps :: TypedAnn -> [CoreExp]
getTypeApps = map (C.Type . snd) . getAnnSubst

toCore :: Expr TypedAnn -> CoreExp
toCore lVar@(a :< S.Var nm) = foldl App (Var var) (getTypeApps a)
  where var = Id nm (polyTyOf lVar) Used
toCore cons@(a :< S.Constructor nm) = foldl App (Var var) (getTypeApps a)
  where var = Id nm (polyTyOf cons) Used
toCore (_ :< S.Case scrut alts) = Case (toCore scrut) (toAlts alts)
toCore (_ :< S.Assign names exprs) = error "assignments must be desugared in blocks"
-- now that the case for lambda also applies type variables, why do we need to do it in an apply?
toCore e@(a :< S.Apply lam args) = foldl App (toCore lam) $ getTypeApps ann ++ (map toCore args)
  where instTy = foldr tFn (snd . unconstrained $ typeOf e) (map typeOf args)
        ann = TyAnn Nothing (S.Type (fromJust' $ instTyOf lam) (Just instTy))

        fromJust' (Just x) = x
        fromJust' _ = error $ show lam
toCore (_ :< S.BinOp op left right) = error "binops should have been desugared to assigns"
toCore (_ :< S.If cond left right) = Case (toCore cond)
  [ ConAlt (Qualified "Prelude" "True") [] (toCore left)
  , ConAlt (Qualified "Prelude" "False") [] (toCore right)
  ]
toCore (lAnn :< S.Lambda bind exp) = let
  vars = map toVar bind
  tyVars = map toTyVar (boundVars $ fromTyAnn lAnn)
  in foldl App (foldr Lambda (toCore exp) (tyVars ++ vars)) (getTypeApps lAnn)
  where
  toVar (a :< S.PVar nm) = Id { varName = nm, idTy = fromTyAnn a, usage = Used }
  isVarPat (_ :< S.PVar _) = True
  isVarPat _ = False

  boundVars (Forall vs _) = vs
  boundVars _ = []

  toTyVar tyvar = TyVar { varName = tyvar, kind = Star }
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
    mkSeq a b = (App (App (Var seq) a) b)
    seq = Id (Qualified "Prelude" "seq") (fmap Internal . generalize $ TVar "a" `tFn` TVar "a") Used
toCore (_ :< S.Literal lit ) = Lit lit

toAlts = map toAlt

toAlt (a :< Wildcard, e) = TrivialAlt $ toCore e
toAlt (_ :< PLit lit, exp) = LitAlt lit $ toCore exp
toAlt (_ :< Destructor n ps, exp) = ConAlt n (map toVar ps) $ toCore exp
  where
  toVar (a :< Wildcard) = Id (Internal "") (fromTyAnn a) NotUsed
  toVar (a :< PVar nm)  = Id nm (fromTyAnn a) Used
  toVar a = error (show a)
