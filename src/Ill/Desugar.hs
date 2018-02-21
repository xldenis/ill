module Ill.Desugar
( module Ill.Desugar
, module X
) where

import           Ill.Prelude

import Ill.Syntax hiding (Expression(..), ty)
import Ill.Syntax.Core as C

import qualified Ill.Syntax as S

import Ill.Desugar.Trait as X
import Ill.Desugar.Cases as X
import Ill.Desugar.BinOp as X
import Ill.Desugar.Administrative as X
import Ill.Desugar.LambdaLift as X

import Control.Monad.State

import Ill.Infer.Monad (Environment)

type Id = Name

{-
  At this point all top level bindings should have cases pushed in,
  traits should have been desugared.

  Data constructors are still kept as implicit bindings
  to variables with the name of the contrsuctor.
-}

defaultPipeline :: Environment -> [Decl TypedAnn] -> [Decl TypedAnn]
defaultPipeline env = desugarTraits env . desugarBinOps >=> pure . simplifyPatterns

compileCore :: [Decl TypedAnn] -> CoreModule
compileCore desugared = declsToCore desugared & normalize . liftModule

declsToCore :: [Decl TypedAnn] -> CoreModule
declsToCore decls = execState (mapM declToCore' decls) (emptyModule)

declToCore' :: Decl TypedAnn -> State CoreModule ()
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
          (TConstructor consNm : args) -> (consNm, (length args, getConstructorType cons, tag))
        ) (zip [0..] conses)
  modify $ \m -> m { constructors = cons' ++ constructors m, types = nm : types m }
  where
  getConstructorType ty = let
    (TConstructor tyCons : tys) = unwrapProduct ty
    retTy = foldl TAp (TConstructor nm) (map TVar args)
    in generalize $ foldr tFn retTy tys
declToCore' (_) = pure ()

{-
  Get the type applications needed for a type annotation
-}
getTypeApps :: TypedAnn -> [CoreExp]
getTypeApps t = map (C.Type . snd) subst
  where
  subst = case join $ subsume <$> (pure . unForall . polyTy $ S.ty t) <*> ((instTy $ S.ty t)) of
    Just l -> filter (\(v, _) -> v `elem` boundVars (polyTy $ S.ty t)) l
    Nothing -> []

  boundVars (Forall vs _) = vs
  boundVars _ = []

  unForall (Forall _ t) = t
  unForall t = t

toCore :: Expr TypedAnn -> CoreExp
toCore lVar@(a :< S.Var nm) = foldl App (Var var) (getTypeApps a)
  where var = Id nm (typeOf lVar) Used
toCore cons@(a :< S.Constructor nm) = foldl App (Var var) (getTypeApps a)
  where var = Id nm (typeOf cons) Used
toCore (_ :< S.Case scrut alts) = Case (toCore scrut) (toAlts alts)
toCore (_ :< S.Assign names exprs) = error "assignments must be desugared in blocks"
toCore (a :< S.Apply lam args) = foldl App (toCore lam) $ (map toCore args) ++ (getTypeApps a)
toCore (_ :< S.BinOp op left right) = error "binops should have been desugared to assigns"
toCore (_ :< S.If cond left right) = Case (toCore cond)
  [ ConAlt "True" [] (toCore left)
  , ConAlt "False" [] (toCore right)
  ]
toCore (lAnn :< S.Lambda bind exp) = let
  vars = map toVar bind
  tyVars = map toTyVar (boundVars $ fromTyAnn lAnn)
  in foldr Lambda (toCore exp) (tyVars ++ vars)
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
    seq = Id "seq" (generalize $ TVar "a" `tFn` TVar "a") Used
toCore (_ :< S.Literal lit ) = Lit lit

toAlts = map toAlt

toAlt (a :< Wildcard, e) = TrivialAlt $ toCore e
toAlt (_ :< PLit lit, exp) = LitAlt lit $ toCore exp
toAlt (_ :< Destructor n ps, exp) = ConAlt n (map toVar ps) $ toCore exp
  where
  toVar (a :< Wildcard) = Id "" (fromTyAnn a) NotUsed
  toVar (a :< PVar nm)  = Id nm (fromTyAnn a) Used
  toVar a = error (show a)
