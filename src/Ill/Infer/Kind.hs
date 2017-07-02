{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Ill.Infer.Kind
( kindsOfAll
) where
import           Ill.Error
import           Control.Monad.Unify
import           Control.Monad.Trans
import           Control.Monad.Except

import qualified Data.HashMap.Strict  as H


import Control.Monad
import Ill.Infer.Monad
import Ill.Syntax (Kind(..), Type(..), Name)

import Data.Maybe

instance Partial (Kind) where
  unknown = KUnknown

  isUnknown (KUnknown u) = Just u
  isUnknown _ = Nothing

  unknowns (KFn f a)    = unknowns f ++ unknowns a
  unknowns Star         = []
  unknowns (KUnknown u) = [u]

  ($?) sub (KFn f a)  = KFn (sub $? f) (sub $? a)
  ($?) sub (Star)     = Star
  ($?) sub k@(KUnknown u) = fromMaybe k $ H.lookup u (runSubstitution sub)

instance Unifiable Check Kind where
  KUnknown u1 =?= KUnknown u2 | u1 == u2 = return ()
  KUnknown u =?= k = u =:= k
  k =?= KUnknown u = u =:= k
  Star =?= Star = return ()
  KFn k1 k2 =?= KFn k3 k4 = do
    k1 =?= k3
    k2 =?= k4
  a =?= b = throwError $ KindUnificationError a b

instance UnificationError Kind MultiError where
  occursCheckFailed t = KindOccursError t

-- kindsOfAll :: _
-- Bind synonym names
-- bind constructor names
-- for each constructor / synonym bind args and solve
kindsOfAll :: [a] -> [(Name, [Name], [Type Name])] -> Check [Kind]
kindsOfAll [] tys = fmap appSubs . liftUnify $ do
  consK <- replicateM (length tys) fresh
  let tyDict = zipWith (\(tyNm, _, _) kVar -> (tyNm, kVar)) tys consK

  bindTypeVariables tyDict $ do
    dataK <- zipWithM (\tyCon (_, args, ts) -> do
      argKs <- replicateM (length args) fresh
      let argDict = zip args argKs
      bindTypeVariables argDict $ do
        solveDataType ts argKs tyCon
      ) consK tys
    return dataK
  where appSubs (ts, sub) = map (starIfUnknown . (sub $?)) ts
        starIfUnknown (KUnknown _) = Star
        starIfUnknown (KFn f a) = KFn (starIfUnknown f) (starIfUnknown a)
        starIfUnknown Star = Star

solveDataType :: [Type Name] -> [Kind] -> Kind -> UnifyT Kind Check Kind
solveDataType ts kargs tyCon = do
  ks <- mapM (infer) ts -- type is a `ap` b
  tyCon =?=  foldr KFn Star kargs


  forM_ ks $ \k -> k =?= Star

  return tyCon

infer :: Type Name -> UnifyT Kind Check Kind
infer (TVar v) = do
  UnifyT . lift $ lookupTypeVariable v
infer (TAp f a) = do
  res <- fresh
  fK  <- infer f
  aK  <- infer a

  -- throwError $ show f ++ show fK ++ " / " ++ show aK
  fK =?= KFn aK res

  return res

infer (TConstructor n) = do
  lookupTypeVariable n

-- infer (Arrow f a) -- should probs desugar to ap before typechecking
-- infer (Trait n t)
-- infer (Constraint ts t)
-- infer (TUnknown u)
