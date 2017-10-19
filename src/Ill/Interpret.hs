{-# LANGUAGE ConstraintKinds       #-}

module Ill.Interpret where

import           Ill.Syntax.Core
-- import Control.Lens
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Function
import           Ill.Syntax.Literal
import           Ill.Syntax.Name
import           Ill.Syntax.Type
import           Ill.Syntax.Pretty (pretty)
type MonadInterpret m = (MonadState Context m, MonadError String m)

data Context = Context
  { boundNames   :: [(Id, CoreExp)]
  , constructors :: [(Id, (Int))] -- cons name, arity, type
  , allocated    :: [(Id, IVal)]
  } deriving (Show)

type IVal = (Id, [CoreExp]) -- an evalutated constructor value

primops =
  [ ("plusInt", (2, plus))
  , ("minusInt", (2, minus))
  , ("gtInt",   (2, gt))
  , ("plusStr", (2, plusStr))
  , ("showInt", (1, showLit))
  ]
  where
  plusStr [Lit (RawString a), Lit (RawString b)] = Lit . RawString $ a ++ b
  showLit [Lit a] = Lit . RawString $ show $ pretty a

  plus = liftToCore (\a b -> Lit . Integer $ a + b)
  minus = liftToCore (\a b -> Lit . Integer $ a - b)
  gt = liftToCore $ \a b -> case a > b of
      True -> Var "True"
      False -> Var "False"

  liftToCore f [Lit (Integer n), Lit (Integer m)] = f n m
  liftToCore f [a, b] = error $ show a ++ show b

{-
  Partially functioning interpreter
  recursive let bindings don't work :( use Y-combinator!
-}

interpret :: MonadInterpret m => CoreExp -> m CoreExp -- ???
interpret l@(Lambda bind exp) = pure l
interpret a@(App _ _) = do -- travel down spine until function is found
  let (f, args) = unwindAppSpine a []
  f' <- interpret f
  go f' args

  where

  go r [] = pure r
  go (Lambda b exp) (arg : args) = do
    e' <- interpret $ substitute (name b, arg) exp
    go e' args
  go (Var "seq") args = do
    when (length args < 2) $ throwError "seq can't be partially applied"
    let ([a, b], remainder) = splitAt 2 args
    interpret a
    res <- interpret b
    go res remainder
  go (Var var) args = do
    cons <- gets constructors
    case var `lookup` cons of
      Nothing -> case var `lookup` primops of
        Nothing -> throwError $ "could not find the name: " ++ var
        Just (arity, f) | length args >= arity -> do
          let (consArgs, remainder) = splitAt arity args
          interpretedArgs <- mapM interpret consArgs

          go (f interpretedArgs) remainder
      Just (arity) | length args >= arity -> do
        nameIx <- gets (length . allocated)
        let (consArgs, remainder) = splitAt arity args
            newValue = ("allocated" ++ show nameIx, (var, consArgs))

        modify $ \ctxt -> ctxt { allocated = newValue  : allocated ctxt }

        go (Var $ "allocated" ++ show nameIx) remainder
  go _ xs = throwError $ "somehow leftover args? " ++ show xs ++ show (a)

  unwindAppSpine (App f a) exps = unwindAppSpine f (a : exps)
  unwindAppSpine a exps         = (a, exps)

interpret (Case scrut alts) = do
  scrut' <- interpret scrut
  selectAlt scrut' alts

  where
  selectAlt (Var nm) alts = do
    ctxt <- get
    case nm `lookup` (ctxt & allocated) of
      Just (tag, bindings) -> firstAlt tag bindings alts
      Nothing -> case nm `lookup` (ctxt & constructors) of
        Just _  -> firstAlt nm [] alts
        Nothing -> throwError $ "idk what to do during a var match: " ++ nm

  firstAlt nm _        (TrivialAlt exp : _) = interpret exp
  firstAlt nm bindVals (ConAlt id binders exp : _) | nm == id = interpret $ foldl
    (\inner bind -> substitute bind inner )
    exp
    (zip (map name binders) bindVals)
  firstAlt nm bindVals (_ : xs) = firstAlt nm bindVals xs
  firstAlt nm _ [] = throwError $ "pattern match failed! " ++ show nm
interpret v@(Var id) = do
  names <- gets boundNames
  case id `lookup` names of
    Just x  -> interpret x
    Nothing -> pure v
interpret (Let bind exp) = interpret $ substitute (bindToSubst bind) exp
  where bindToSubst (NonRec n exp) = (name n, exp)
interpret t@(Type ty) = pure t
interpret l@(Lit lit) = pure l

substBoundName :: MonadInterpret m => Bind Var -> CoreExp -> m CoreExp
substBoundName (NonRec n arg) exp = interpret $ substitute (name n, arg) exp

withBoundName :: MonadInterpret m => Bind Var -> (m CoreExp) -> m CoreExp
withBoundName (NonRec n exp) f = do
  exp' <- interpret exp
  names <- gets boundNames
  when (usage n == Used) $ modify $ \ctxt -> ctxt { boundNames =  (name n, exp') : boundNames ctxt }
  res <- f
  modify $ \ctxt -> ctxt { boundNames = names }
  pure res

