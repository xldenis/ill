{-# LANGUAGE ConstraintKinds       #-}

module Ill.Interpret where

import           Ill.Syntax.Core

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Function
import           Ill.Syntax.Literal
import           Ill.Syntax.Name
import           Ill.Syntax.Type
import           Ill.Syntax.Pretty (pretty)

import Ill.Error (rethrow)
import Ill.Syntax.Pretty hiding (when)

type MonadInterpret m = (MonadState Context m, MonadError Error m)

data Context = Context
  { boundNames   :: [(Id, CoreExp)]
  , constructors :: [(Id, Int)] -- cons name, arity
  , allocated    :: [(Id, IVal)]
  , allocationStats :: [(Id, Int)]
  } deriving (Show)

type IVal = (Id, [CoreExp]) -- an evalutated constructor value

primops =
  [ ("plusInt", (2, plus))
  , ("minusInt", (2, minus))
  , ("gtInt",   (2, gt))
  , ("ltInt", (2, lt))
  , ("plusStr", (2, plusStr))
  , ("showInt", (1, showLit))
  ]
  where
  plusStr [Lit (RawString a), Lit (RawString b)] = Lit . RawString $ a ++ b
  plusStr xs = error $ show $ pretty xs
  showLit [Lit a] = Lit . RawString $ show $ pretty a

  plus = liftToCore (\a b -> Lit . Integer $ a + b)
  minus = liftToCore (\a b -> Lit . Integer $ a - b)
  gt = liftToCore $ \a b -> case a > b of
      True -> Var "True"
      False -> Var "False"

  lt = liftToCore $ \a b -> case a < b of
      True -> Var "True"
      False -> Var "False"

  liftToCore f [Lit (Integer n), Lit (Integer m)] = f n m
  liftToCore f [a, b] = error $ show a ++ show b

{-
  Partially functioning interpreter
  recursive let bindings don't work :( use Y-combinator!
-}

data Error
  = ElabError String
  | WithLoc CoreExp Error
  deriving (Show)

instance Pretty Error where
  pretty (WithLoc location error) = vcat $
    [ pretty "Error in the expression:"
    , pretty location
    , (nest 2 $ pretty error)
    ]
  pretty (ElabError string) = pretty "error during execution" <+> pretty string

interpret :: MonadInterpret m => CoreExp -> m CoreExp -- ???
interpret exp = rethrow (WithLoc exp) (interpret' exp)

interpret' :: MonadInterpret m => CoreExp -> m CoreExp -- ???
interpret' l@(Lambda bind exp) = pure l
interpret' a@(App _ _) = do -- travel down spine until function is found
  let (f, args) = unwindAppSpine a []
  f' <- interpret f
  go f' args

  where

  go r [] = pure r
  go (Lambda b exp) (arg : args) = do
    e' <- interpret $ substitute (name b, arg) exp
    go e' args
  go (Var "seq") args = do
    when (length args < 2) $ throwError . ElabError $ "seq can't be partially applied"
    let ([a, b], remainder) = splitAt 2 args
    interpret a
    res <- interpret b
    go res remainder
  go (Var var) args = do
    cons <- gets constructors
    case var `lookup` cons of
      Nothing -> case var `lookup` primops of
        Nothing -> do
          throwError . ElabError $ "could not find the primitive operation: " ++ var  ++ " " ++ (show $ pretty a)
        Just (arity, f) | length args >= arity -> do
          let (consArgs, remainder) = splitAt arity args
          interpretedArgs <- mapM interpret consArgs
          go (f interpretedArgs) remainder
      Just arity | length args >= arity -> do
        let (consArgs, remainder) = splitAt arity args
        value <- allocateValue var consArgs

        go value remainder
  go _ xs = throwError . ElabError $ "somehow leftover args? " ++ show xs ++ show (a)

  unwindAppSpine (App f a) exps = unwindAppSpine f (a : exps)
  unwindAppSpine a exps         = (a, exps)

  allocateValue consName args = do
    nameIx <- gets (length . allocated)
    let newValue = ("allocated" ++ show nameIx, (consName, args))

    stats' <- updateDict (+) (consName, 1) <$> gets allocationStats
    modify $ \ctxt -> ctxt { allocated = newValue  : allocated ctxt, allocationStats = stats' }
    return $ Var $ "allocated" ++ show nameIx

interpret' (Case scrut alts) = do
  scrut' <- interpret scrut
  case scrut' of
    Var nm -> do
      ctxt <- get
      case nm `lookup` (ctxt & allocated) of
        Just (tag, bindings) -> firstAlt tag bindings alts
        Nothing -> case nm `lookup` (ctxt & constructors) of
          Just _  -> firstAlt nm [] alts
          Nothing -> throwError . ElabError $ "somehow this variable was not allocated nor a cons: " ++ nm
    x -> error (show x)

  where

  firstAlt nm _        (TrivialAlt exp : _) = do
    result <-  interpret exp
    when (Var "failedPattern" == result) $ throwError $ ElabError "pattern match failed :("
    return result
  firstAlt nm bindVals (ConAlt id binders exp : _) | nm == id = interpret $ foldl
    (\inner bind -> substitute bind inner )
    exp
    (zip (map name binders) bindVals)
  firstAlt nm bindVals (_ : xs) = firstAlt nm bindVals xs
  firstAlt nm _ [] = throwError . ElabError $ "pattern match failed! " ++ show nm
interpret' v@(Var id) = do
  names <- gets boundNames
  case id `lookup` names of
    Just x  -> interpret x
    Nothing -> pure v
interpret' a@(Let bind exp) = do
  bindings <- gets boundNames
  let newName = "boundName" ++ show (length bindings)
      NonRec n val = bind
      val' = substitute (name n, Var newName) val
  modify $ \ctxt -> ctxt { boundNames = (newName, val') : (boundNames ctxt) }

  interpret $ substitute (name n, val') exp

  where bindToSubst (NonRec n exp) = (name n, exp)
interpret' t@(Type ty) = pure t
interpret' l@(Lit lit) = pure l

updateDict f (k, v) ((dK, dV):dict) | k == dK = (k, f v dV) : dict
                                  | otherwise = (dK, dV) : updateDict f (k, v) dict
updateDict f (k, v) [] = [(k, v)]

substBoundName :: MonadInterpret m => Bind Var -> CoreExp -> m CoreExp
substBoundName (NonRec n arg) exp = interpret $ substitute (name n, arg) exp
