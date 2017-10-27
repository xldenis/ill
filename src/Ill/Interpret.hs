module Ill.Interpret where

import           Ill.Syntax.Core
import           Ill.Syntax hiding (Expression(..))

import           Control.Monad.Except
import           Control.Monad.State

import           Data.IORef
import           Data.List (find)
import           Data.Function

type Thunk = () -> IO Value

data Value
  = VLit Literal
  | VClosure (Thunk -> IO Value)
  | VConstructed String [IORef Thunk]

showish (VLit lit) = pretty lit
showish (VClosure _) = pretty "<<closure>>"
showish (VConstructed _ vars) = pretty "<<constructed>>"

update :: IORef Thunk -> Value -> IO ()
update ref val = do
  writeIORef ref (\() -> return val)
  return ()

force :: IORef Thunk -> IO Value
force ref = do
  th <- readIORef ref
  v  <- th ()
  update ref v
  return v

data Env = Env { names :: [(Name, IORef Thunk)], constructors :: [(Name, Int)] }

mkEnvForModule :: [(Name, Int)] -> [Bind Var] -> IO Env
mkEnvForModule cons funcs = do
  thunks <- replicateM (length funcs) (newIORef undefined)
  let funcs' = map (\(NonRec var exp) -> (name var, exp)) funcs
      names  = zipWith (\(nm, _) thunk -> (nm, thunk)) funcs' thunks
      env    = Env names cons

  zipWithM (\(name, value) thunk -> do
    val <- eval env value
    update thunk val
    ) funcs' thunks

  return $ env

primops =
  [ ("showInt", (1, showInt))
  , ("plusStr", (2, plusStr))
  , ("minusInt", (2, minusInt))
  , ("plusInt", (2, plusInt))
  , ("gtInt",   (2, gtInt))
  , ("multInt", (2, multInt))
  -- , ("divInt", (2, divInt))
  , ("eqInt", (2, eqInt))
  , ("ltInt", (2, ltInt))
  , ("leqInt", (2, leqInt))
  , ("geqInt", (2, geqInt))
  , ("maxInt", (2, maxInt))
  , ("minInt", (2, minInt))
  ]

  where
  multInt  = liftBinInt $ \a b -> VLit . Integer $ a * b
  maxInt   = liftBinInt $ \a b -> VLit . Integer $ max a b
  minInt   = liftBinInt $ \a b -> VLit . Integer $ min a b
  minusInt = liftBinInt $ \a b -> VLit . Integer $ a - b
  plusInt  = liftBinInt $ \a b -> VLit . Integer $ a + b

  eqInt  = liftBinInt $ \a b -> liftCmp $ a == b
  leqInt = liftBinInt $ \a b -> liftCmp $ a <= b
  geqInt = liftBinInt $ \a b -> liftCmp $ a >= b
  ltInt  = liftBinInt $ \a b -> liftCmp $ a < b
  gtInt  = liftBinInt $ \a b -> liftCmp $ a > b


  plusStr [VLit (RawString a), VLit (RawString b)] = VLit . RawString $ a ++ b
  showInt [VLit (Integer a)] = VLit $ RawString (show a)

  liftBinInt f [VLit (Integer a), VLit (Integer b)] = f a b
  liftBinInt _ _ = error "ruh roh spaghettioes"

  liftCmp b = case b of
    True -> VConstructed "True" []
    False -> VConstructed "False" []

mkThunk :: Env -> Name -> CoreExp -> (Thunk -> IO Value)
mkThunk env nm exp = \thunk -> do
  thunk' <- newIORef thunk

  eval (env { names = (nm, thunk') : names env }) exp

eval :: Env -> CoreExp -> IO Value
eval env (Var n) = do
  case n `lookup` (env & names) of
    Nothing -> case n `lookup` (env & constructors) of
                  Just 0 -> return $ VConstructed n []
                  Nothing -> error $ "failed to lookup " ++ n -- this is where i need to sneak in a subsitution for primops
    Just thunk -> force thunk
eval env (Lambda n exp) = return $ VClosure (mkThunk env (name n) exp)
eval env (Lit lit) = return $ VLit lit
eval env a@(App _ _) = do
  let unwound = unwindSpineStack a []

  evalApp env unwound
  where

  unwindSpineStack (App f a) stack = unwindSpineStack f (a : stack)
  unwindSpineStack a         stack = a : stack

  evalApp env (Var v : stack)
    | isPrimop v = do -- check for partial application of primop!!
      let Just (arity, opF) = v `lookup` primops
          args = take arity stack
      args' <- mapM (eval env) args

      return $ opF args'
    | isConstructor v = do
      let Just arity = v `lookup` (env & constructors)
          args = take arity stack

      args' <- forM args $ \arg -> newIORef $ \() -> eval env arg

      return $ VConstructed v args'

  evalApp env (v : arg : stack) = do
    val <- eval env v
    go env val (arg : stack)

    where
    go env (VClosure closure) (arg : stack) = do
      result <- closure $ \() -> eval env arg

      go env result stack
    go env res [] = pure res

  isPrimop v = maybeToBool $ v `lookup` primops

  isConstructor v = v `lookup` (env & constructors) & maybeToBool

  maybeToBool x = case x of
    Just _ -> True
    Nothing -> False

eval env (Case scrut alts) = do
  scrut' <- eval env scrut
  when (isLit scrut') $ error "sorry i dont support this yet :("
  let VConstructed tag bound = scrut'
  let Just matchedAlt = find (matchCons tag) alts

  evalAlt env bound matchedAlt
  where

  isLit (VLit _) = True
  isLit _ = False

  matchCons tag (TrivialAlt _) = True
  matchCons tag (ConAlt nm _ _) = nm == tag

  evalAlt env _ (TrivialAlt exp) = eval env exp
  evalAlt env bound (ConAlt _ vars exp) = do
    let env' = env { names = zipWith (\v b -> (name v, b)) vars bound ++ (names env) }
    eval env' exp

eval env (Let (NonRec nm arg) exp) = do
  thunk <- newIORef undefined
  let env' = env { names = (name nm, thunk) : names env }
  writeIORef thunk $ \() -> eval env' arg

  eval env' exp

