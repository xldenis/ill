module Thrill.Interpret where

import           Thrill.Prelude

import           Thrill.Syntax.Core as Core hiding (constructors)
import           Thrill.Syntax hiding (Expression(..))
import           Thrill.Syntax.Pretty ((<+>), hsep)

import           Control.Monad.Except
import           Control.Monad.State

import           Data.IORef
import           Data.Fixed

type Thunk = () -> IO Value

data Value
  = VLit Literal
  | VClosure (Thunk -> IO Value)
  | VConstructed QualifiedName [IORef Thunk]
  | VPrimop String Int ([Value] -> Value)
  | VConstructor QualifiedName Int

showish (VLit lit) = pretty lit
showish (VClosure _) = pretty "<<closure>>"
showish (VConstructed n vars) = pretty "<<constructed:" <+> pretty n <+> pretty ">>"
showish (VPrimop n _ _) = pretty "<<primop:" <+> pretty n <+> pretty ">>"
showish (VConstructor n arty) = pretty "<<constructor:" <+> pretty n <+> pretty "arity:" <+> pretty arty <+> pretty ">>"

update :: IORef Thunk -> Value -> IO ()
update ref val = do
  writeIORef ref (\() -> return val)
  return ()

update' :: IORef Thunk -> Thunk -> IO ()
update' ref thunk = do
  writeIORef ref thunk
  return ()

force :: IORef Thunk -> IO Value
force ref = do
  th <- readIORef ref
  v  <- th ()
  update ref v
  return v

data Env = Env { names :: [(QualifiedName, IORef Thunk)], constructors :: [(QualifiedName, Int)] }

mkEnvForModule :: [(QualifiedName, Int)] -> [Bind Var] -> IO Env
mkEnvForModule cons funcs = do
  thunks <- replicateM (length funcs) (newIORef $ error "error setting up interpreter context")
  let funcs' = map (\(NonRec var exp) -> (name var, exp)) funcs
      names  = zipWith (\(nm, _) thunk -> (nm, thunk)) funcs' thunks
      env    = Env names cons

  zipWithM (\(name, value) thunk -> do
    update' thunk $ \() -> eval env value
    ) funcs' thunks

  return $ env

primops =
  [ ("showInt",      (1, showInt))
  , ("plusStr",      (2, plusStr))
  , ("minusInt",     (2, minusInt))
  , ("plusInt",      (2, plusInt))
  , ("gtInt",        (2, gtInt))
  , ("multInt",      (2, multInt))
  , ("divInt",       (2, divInt))
  , ("eqInt",        (2, eqInt))
  , ("ltInt",        (2, ltInt))
  , ("leqInt",       (2, leqInt))
  , ("geqInt",       (2, geqInt))
  , ("maxInt",       (2, maxInt))
  , ("minInt",       (2, minInt))
  , ("modInt",       (2, modInt))
  , ("showDouble",   (1, showDouble))
  , ("plusStr",      (2, plusStr))
  , ("minusDouble",  (2, minusDouble))
  , ("plusDouble",   (2, plusDouble))
  , ("gtDouble",     (2, gtDouble))
  , ("multDouble",   (2, multDouble))
  , ("divDouble",    (2, divDouble))
  , ("eqDouble",     (2, eqDouble))
  , ("ltDouble",     (2, ltDouble))
  , ("leqDouble",    (2, leqDouble))
  , ("geqDouble",    (2, geqDouble))
  , ("maxDouble",    (2, maxDouble))
  , ("minDouble",    (2, minDouble))
  , ("modDouble",    (2, modDouble))
  ]

  where
  multInt  = liftBinInt $ \a b -> VLit . Integer $ a * b
  maxInt   = liftBinInt $ \a b -> VLit . Integer $ max a b
  minInt   = liftBinInt $ \a b -> VLit . Integer $ min a b
  minusInt = liftBinInt $ \a b -> VLit . Integer $ a - b
  plusInt  = liftBinInt $ \a b -> VLit . Integer $ a + b
  divInt   = liftBinInt $ \a b -> VLit . Integer $ a `div` b
  modInt   = liftBinInt $ \a b -> VLit . Integer $ a `mod` b

  eqInt  = liftBinInt $ \a b -> liftCmp $ a == b
  leqInt = liftBinInt $ \a b -> liftCmp $ a <= b
  geqInt = liftBinInt $ \a b -> liftCmp $ a >= b
  ltInt  = liftBinInt $ \a b -> liftCmp $ a < b
  gtInt  = liftBinInt $ \a b -> liftCmp $ a > b

  multDouble  = liftBinDouble $ \a b -> VLit . Double $ a * b
  maxDouble   = liftBinDouble $ \a b -> VLit . Double $ max a b
  minDouble   = liftBinDouble $ \a b -> VLit . Double $ min a b
  minusDouble = liftBinDouble $ \a b -> VLit . Double $ a - b
  plusDouble  = liftBinDouble $ \a b -> VLit . Double $ a + b
  divDouble   = liftBinDouble $ \a b -> VLit . Double $ a / b
  modDouble   = liftBinDouble $ \a b -> VLit . Double $ a `mod'` b

  eqDouble  = liftBinDouble $ \a b -> liftCmp $ a == b
  leqDouble = liftBinDouble $ \a b -> liftCmp $ a <= b
  geqDouble = liftBinDouble $ \a b -> liftCmp $ a >= b
  ltDouble  = liftBinDouble $ \a b -> liftCmp $ a < b
  gtDouble  = liftBinDouble $ \a b -> liftCmp $ a > b

  plusStr [VLit (RawString a), VLit (RawString b)] = VLit . RawString $ a ++ b
  showInt [VLit (Integer a)] = VLit $ RawString (show a)
  showDouble [VLit (Double  a)] = VLit $ RawString (show a)

  liftBinInt f [VLit (Integer a), VLit (Integer b)] = f a b
  liftBinInt _ args = error . show $ pretty "ruh roh spaghettioes" <+> hsep (map showish args)

  liftBinDouble f [VLit (Double a), VLit (Double b)] = f a b
  liftBinDouble _ args = error . show $ pretty "ruh roh spaghettioes" <+> hsep (map showish args)

  liftCmp b = case b of
    True  -> VConstructed (Qualified "Prelude" "True")  []
    False -> VConstructed (Qualified "Prelude" "False") []

mkThunk :: Env -> QualifiedName -> CoreExp -> (Thunk -> IO Value)
mkThunk env nm exp = \thunk -> do
  thunk' <- newIORef thunk

  eval (env { names = (nm, thunk') : names env }) exp

eval :: Env -> CoreExp -> IO Value
eval env (Var v) =
  case lookupVar <|> lookupConstructor <|> lookupPrimOp of
    Nothing -> error $ "failed to lookup " ++ (qualName n)
    Just x -> x
  where
  n = varName v
  lookupVar = n `lookup` (env & names) >>= return . force
  lookupConstructor = n `lookup` (env & constructors) >>= \arity -> case arity of
    0 -> return . return $ VConstructed n []
    arity -> return . return $ VConstructor n arity
  lookupPrimOp = (qualName n) `lookup` primops >>= \(arity, f) -> (return . return $ VPrimop (qualName n) arity f)
eval env (Lambda (TyVar{}) exp) = eval env exp
eval env (Lambda n exp) = return $ VClosure (mkThunk env (name n) exp)
eval env (Lit lit) = return $ VLit lit
eval env a@(App _ _) = do
  let (v : stack) = unwindSpineStack a []
  val <- eval env v
  evalApp env val stack

  where

  unwindSpineStack (App f (Core.Type _)) stack = unwindSpineStack f stack
  unwindSpineStack (App f a) stack = unwindSpineStack f (a : stack)
  unwindSpineStack a         stack = a : stack

  evalApp env (VConstructor n arity) stack | length stack >= arity = do
    let (args, remainder) = splitAt arity stack
    args' <- forM args $ \arg -> newIORef $ \() -> eval env arg

    evalApp env (VConstructed n args') remainder
  evalApp env (VPrimop opName arity opF) stack = do -- check for partial application
    let (args, res) = splitAt arity stack
    args' <- mapM (eval env) args
    evalApp env (opF args') res

  evalApp env (VClosure closure) (arg : stack) = do
    result <- closure $ \() -> eval env arg
    evalApp env result stack
  evalApp env res [] = pure res
  evalApp env res args = error . show $ showish res <+> pretty args

eval env (Case scrut alts) = do
  scrut' <- eval env scrut
  fromJust . asum $ map (evalAlt env scrut') alts
  where

  evalAlt env (VLit l) (LitAlt lit exp) | lit == l = do
    return $ eval env exp
  evalAlt env (VConstructed tag bound) (ConAlt ctag vars exp) | ctag == tag = do
    let env' = env { names = zipWith (\v b -> (name v, b)) vars bound ++ (names env) }
    return $ eval env' exp
  evalAlt env _ (TrivialAlt exp) = return $ eval env exp
  evalAlt _ scrut alt = Nothing

eval env (Let (NonRec nm arg) exp) = do
  thunk <- newIORef undefined
  let env' = env { names = (name nm, thunk) : names env }
  update' thunk $ \() -> eval env' arg

  eval env' exp
eval env expr = error . show $ pretty expr
