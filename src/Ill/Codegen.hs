{-# LANGUAGE RecursiveDo, OverloadedStrings, ConstraintKinds, ScopedTypeVariables #-}
module Ill.Codegen where

import Control.Monad (forM)
import Data.String

import           Ill.Syntax.Core
import           Ill.Syntax.Literal
import           Ill.Syntax.Name
import           Ill.Syntax.Type

import           LLVM.AST.AddrSpace
import           LLVM.IRBuilder
import           LLVM.Pretty
import           LLVM.AST.Operand
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Type as T
import qualified LLVM.AST.Typed as T

import qualified Control.Monad as M
import qualified LLVM.AST.IntegerPredicate as AST
import           Ill.Prelude hiding (void)
import           Data.Word
import           Data.Maybe
import           Data.Char
import           Data.List (find)
import           Control.Monad.Fix
import           Control.Monad.Reader
import           Control.Monad.State (gets)
import           Ill.Syntax.Pretty
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

import Debug.Trace

import qualified Ill.Syntax.Builtins as Builtins
import Data.Text.Lazy (pack)

{-
  init doesnt work
-}
prettyModule mod =  ppllvm $ compileModule mod

insertvalue :: MonadIRBuilder m => Operand -> Operand -> [Word32] -> m Operand
insertvalue agg el is = emitInstr (T.typeOf agg) $ AST.InsertValue agg el is []

extractvalue :: MonadIRBuilder m => T.Type -> Operand -> [Word32] -> m Operand
extractvalue ty agg is = emitInstr (ty) $ AST.ExtractValue agg is []

byte :: Applicative f => Integer -> f Operand
byte = pure . ConstantOperand . C.Int 8

malloc :: MonadIRBuilder m => AST.Operand -> m AST.Operand
malloc size = do
  call (ConstantOperand $ C.GlobalReference mallocTy "malloc") [(size, [])]
  where mallocTy = ptr $ T.FunctionType (ptr T.i8) [T.i64] False

data CodegenState = CS
  { globalInfo :: [(Id, BindingInfo)]
  , localInfo :: [(Id, AST.Operand)]
  } deriving (Show, Eq)

updateLocals f (CS c l) = CS c (f l)

primInt = ptr $ T.NamedTypeReference "Int"
primBool = ptr $ T.NamedTypeReference "Bool"
primStr  = ptr $ T.NamedTypeReference "String"

compileModule :: CoreModule -> AST.Module
compileModule mod =  buildModule "example" . (flip runReaderT (CS infoMap [])) $ mdo
  extern "malloc" [T.i64] (ptr T.i8)
  extern "ltInt" [primInt, primInt] primBool
  extern "gtInt" [primInt, primInt] primBool
  extern "eqInt" [primInt, primInt] primBool
  extern "geqInt" [primInt, primInt] primBool
  extern "leqInt" [primInt, primInt] primBool
  extern "minusInt" [primInt, primInt] primInt
  extern "plusInt" [primInt, primInt] primInt
  extern "plusStr" [primStr, primStr] primStr
  extern "showInt" [primInt] primStr

  typedef "String" (Just $ T.StructureType False [T.i64, ptr $ T.i8])

  defMkDouble
  defMkInt
  apply1

  forM (types mod) $ \nm -> typedef (fromString nm) $ Just $ T.StructureType False [T.i64]
  forM (constructors mod) compileConstructor
  forM (bindings mod) compileBinding

  where
  isLambda (NonRec _ Lambda{}) = True
  isLambda _ = False

  infoMap =
    map collectBindingInfo (bindings mod) ++
    map collectConstructorInfo (constructors mod) ++
    builtinInfo

data BindingInfo = Info
  { arity :: Int
  , argTys :: [T.Type]
  , retTy :: T.Type
  , operand :: AST.Operand
  } deriving (Show, Eq)

builtinInfo :: [(Id, BindingInfo)]
builtinInfo = map go Builtins.primitives
  where
  go (nm, ty) = let
    arity = length args
    tys = unwrapFnType ty
    args = map llvmArgType $ init tys
    ret = llvmArgType $ last tys
    op  = ConstantOperand $ C.GlobalReference (ptr $ T.FunctionType ret args False) (fromString nm)
    in (nm, Info arity args ret op)

collectConstructorInfo :: (Name, (Int, Type Name)) -> (Id, BindingInfo)
collectConstructorInfo (nm, (arity, ty)) = let
  tys = unwrapFnType ty
  args = map llvmArgType $ init tys
  ret  = llvmArgType $ last tys

  op = ConstantOperand $ C.GlobalReference (ptr $ T.FunctionType ret args False) (fromString nm)

  in (nm, Info arity args ret op)

collectBindingInfo :: Bind Var -> (Id, BindingInfo)
collectBindingInfo (NonRec v b) = let
  arity = length . snd $ unwrapLambda b
  tys   = unwrapN arity (idTy v)
  args  = map llvmArgType $ init tys
  ret   = llvmArgType $ last tys
  op    = ConstantOperand $ C.GlobalReference (ptr $ T.FunctionType ret args False) (fromString $ varName v)
  in (varName v, Info arity args ret op)

defMkDouble :: ModuleM m => m Operand
defMkDouble = do
  let dType = ptr $ T.NamedTypeReference (fromString "Double")
  typedef (fromString "Double") (Just $ T.StructureType False [T.i64, T.double])
  function "mkDouble" [(T.double, "d")] (dType) $ \[d] -> do
    block `named` "entry" ; do
      memPtr <- malloc =<< (int64 $ 8 + 8)
      iPtr <- bitcast memPtr (dType)
      i <- load iPtr 8
      i' <- insertvalue i d [1]
      store iPtr 8 i'
      ret iPtr

mkDouble d = call (AST.LocalReference mkDoubleTy "mkDouble") [(d, [])]
  where mkDoubleTy = ptr $ T.FunctionType (ptr $ T.NamedTypeReference (fromString "Double")) [T.double] False

defMkInt :: ModuleM m => m Operand
defMkInt = do
  let iType = ptr $ T.NamedTypeReference (fromString "Int")
  typedef (fromString "Int") (Just $ T.StructureType False [T.i64, T.i64])
  function "mkInt" [(T.i64, "d")] (iType) $ \[d] -> do
    block `named` "entry" ; do
      memPtr <- malloc =<< (int64 $ 8 + 8)
      iPtr <- bitcast memPtr iType
      i <- load iPtr 8
      i' <- insertvalue i d [1]
      store iPtr 8 i'
      ret iPtr

mkInt d = call (AST.ConstantOperand $ C.GlobalReference mkIntTy "mkInt") [(d, [])]
  where mkIntTy = ptr $ T.FunctionType (ptr $ T.NamedTypeReference (fromString "Int")) [T.i64] False

closureType = ptr $ T.StructureType False (ptr T.i8 : T.i8 : [T.ArrayType 1 $ ptr T.i8])

apply1 :: ModuleM m => m Operand
apply1 = do
  function "apply1" [(closureType , "closure"), (ptr T.i8, "argPtr")] (ptr T.i8) $ \[closurePtr, argPtr] -> do
    block `named` "entry"

    arityPtr <- gep closurePtr $ int32 0 ++ int32 1
    arity <- load argPtr 8
    remainingArity <- zext arity T.i32
    cond <- icmp AST.EQ remainingArity (ConstantOperand $ C.Int 32 0)
    condBr cond "Arity 1" "default"

    block `named` "Arity 1"; do
      closureArgPtr <- gep closurePtr $ int32 0 ++ int32 2 ++ int32 0
      store closureArgPtr 8 argPtr
      voidPtrPtr <- gep closurePtr $ int32 0 ++ int32 0
      voidPtr <- load voidPtrPtr 8
      fPtr <- bitcast voidPtr (ptr $ T.FunctionType (ptr $ T.i8) [T.typeOf closurePtr] False)
      retVal <- call fPtr [(closurePtr, [])]
      ret retVal
    block `named` "default"; do
      arity' <- sub remainingArity =<< int32 1
      closureArgPtr <- gep closurePtr $ int32 0 ++ int32 2 ++ [arity']
      store closureArgPtr 8 argPtr
      retVal <- bitcast closurePtr (ptr $ T.i8)
      ret retVal

compileConstructor :: MonadModuleBuilder m => (Name, (Int, Type Name)) -> m ()
compileConstructor (nm, (_, ty)) = do
  typedef (fromString nm) (Just $ T.StructureType False llvmArgTys)

  M.void $ function (fromString $ nm) funArgs (retTy) $ \args -> do
    block `named` "entryC" ; do
      voidPtr <- malloc (sizeofType $ ptr consTy)
      memPtr <- bitcast voidPtr (ptr consTy)
      val <- load memPtr 8
      header <- int64 0
      headerVal <- insertvalue val header [0]
      built <- M.foldM (\prev (ix, arg) -> insertvalue prev arg [ix]) val (zip [1..] args)

      store memPtr 8 built
      retPtr <- bitcast memPtr retTy
      ret retPtr
  where
  consTy = T.NamedTypeReference $ fromString nm
  llvmArgTys = T.i64 : llvmArgTy'
  llvmArgTy' = map llvmArgType argTys
  argTys = init unwrapped
  unwrapped = unwrapFnType ty
  retTy = llvmArgType (last unwrapped)
  funArgs = zip llvmArgTy' (repeat (fromString "a"))

ptr x = T.PointerType x (AddrSpace 0)

llvmArgType (TVar nm) = ptr $ T.i8
llvmArgType (TConstructor nm) = ptr $ T.NamedTypeReference (fromString nm)
llvmArgType ap@(TAp _ _) = let
  cons : _ = unwrapProduct ap
  in llvmArgType cons
llvmArgType (Forall _ t) = llvmArgType t
llvmArgType f@(Arrow _ _) =
  closureType
  where
  llvmArgTy' = reverse $ map llvmArgType argTys
  argTys = init unwrapped
  unwrapped = unwrapFnType f
llvmArgType f@(TAp (TAp (TConstructor "->") a) b) =
  closureType
  where
  llvmArgTy' = reverse $ map llvmArgType argTys
  argTys = init unwrapped
  unwrapped = unwrapFnType f
llvmArgType t = error $ show t

type CodegenM m = (MonadFix m, MonadIRBuilder m,     MonadReader CodegenState m)
type ModuleM  m = (MonadFix m, MonadModuleBuilder m, MonadReader CodegenState m)

compileBinding :: ModuleM m => Bind Var -> m ()
compileBinding (NonRec v l) | varName v == "main" = compileBinding (NonRec (v { varName = "module_main" }) l)
compileBinding (NonRec nm l) = do
  when (length argVars > 0) $ void $ mkClosureCall (length argVars) nm

  function (fromString $ varName nm) args retTy $ \args -> mdo
    block `named` "entry" ; do
      let dict = zipWith (\v op -> (fromString $ varName v, op)) argVars args
      local (updateLocals (dict ++)) $ do
        retVal <- compileBody body
        bitcast retVal retTy >>= ret

  pure ()

  where
  args = map (\var -> (llvmArgType (idTy var), fromString $ varName var)) argVars
  retTy = llvmArgType $ last $ unwrapN (length argVars) $ idTy nm
  (body, argVars) = unwrapLambda l

unwrapLambda :: Core Var -> (Core Var, [Var])
unwrapLambda (Lambda b@(Id{}) e) = (b :) <$> unwrapLambda e
unwrapLambda (Lambda _ e) = unwrapLambda e
unwrapLambda e = (e, [])

compileBody :: forall m . CodegenM m => CoreExp -> m AST.Operand
compileBody (Lit (Double d)) = mkDouble =<< double d
compileBody (Lit (Integer i)) = mkInt =<< int64 i
compileBody (Lit (RawString i)) = do
  let charOrds = (BS.unpack $ BS8.pack i) ++ [0]
  let chars = map (C.Int 8 . fromIntegral) charOrds
  arr <- array chars

  voidPtr <- malloc =<< (int64 . fromIntegral $ length chars)
  arrPtr  <- bitcast voidPtr (ptr $ T.typeOf arr)
  store arrPtr 8 arr

  str <- struct Nothing False $ [C.Int 64 . fromIntegral $ length i, C.Null (ptr $ T.i8)]
  charPtr <- bitcast arrPtr (ptr $ T.i8)
  fullStruct <- insertvalue str charPtr [1]

  voidPtr <- malloc (sizeofType . ptr $ T.typeOf fullStruct)
  memPtr <- bitcast voidPtr (ptr $ T.typeOf fullStruct)
  store memPtr 8 fullStruct

  bitcast memPtr (ptr $ T.NamedTypeReference "String")
compileBody (Var v) = do
  dict <- reader localInfo
  globalDefs <- reader globalInfo

  case varName v `lookup` globalDefs of
    Just i | arity i == 0 -> knownCall i v []
    Just i -> buildClosure i v
    Nothing ->
      pure . nameAssert dict $ varName v `lookup` dict
  where
  nameAssert _ (Just nm) = nm
  nameAssert dict Nothing = error $ "no " ++ show v ++ show dict ++ " found."
compileBody l@(Let (NonRec v e) exp) = do -- figure out how to handle recursive let bindings
  cE <- compileBody e

  local (updateLocals ((varName v, cE) :)) $ compileBody exp
compileBody (Case scrut alts) = mdo
  scrutOp <- compileBody scrut

  tagPtr <- emitInstr (ptr T.i64) (AST.GetElementPtr False scrutOp (int32 0 ++ int32 0) [])
  tag <- load tagPtr 8

  switch tag defAlt alts'

  (alts', phis) <- unzip <$> M.zipWithM (compileAlt retBlock scrutOp) [1..] (filter isConAlt alts)
  (defAlt, maybeBody) <- fromJust $ (compileDefaultAlt retBlock) <$> (find isTrivialAlt alts) <|> pure (defaultBranch retBlock)

  retBlock <- block `named` "switch_return" ; do
    phi $ maybeToList maybeBody ++ phis
  where
  fromJust' (Just x) = x
  fromJust' _ = error (show alts)

  defaultBranch retB = do
    blk <- block `named` "default_branch" ; do
      unreachable
    pure (blk, Nothing)

  compileDefaultAlt retB (TrivialAlt (App (Var v) _)) | varName v == "failedPattern" = do
    blk <- block `named` "trivial_branch" ; do
      unreachable
    pure (blk, Nothing)
  compileDefaultAlt retB (TrivialAlt b) = do
    blk <- block `named` "trivial_branch" ; do
      body <- compileBody b
      br retB

      mbb <- liftIRState $ gets builderBlock

      let label = partialBlockName $ fromJust mbb
      pure (blk, Just (body, label))

  compileAlt :: AST.Name -> Operand -> Integer -> Alt Var -> m ((C.Constant, AST.Name), (Operand, AST.Name))
  compileAlt retB scrut i c@(ConAlt cons binds b) = do
    block <- block `named` (fromString $ "branch" ++ cons)
    scrutPtr <- bitcast scrut $ ptr $ T.NamedTypeReference (fromString cons)
    scrut' <- load scrutPtr 8

    (Info _ argTys _ _) <- reader (\s -> fromJust $ cons `lookup` globalInfo s)

    bindVals <- catMaybes <$> (forM (zip3 [1..] binds argTys) $ \(ix, v, ty) -> do
      case usage v of
        Used -> do
          field <- extractvalue (ty) scrut' [ix]
          return $ Just (varName v, field)
        NotUsed -> pure Nothing)

    let tag = C.Int 64 i
    val <- local (updateLocals (bindVals ++)) $ compileBody b
    mbb <- liftIRState $ gets builderBlock

    let label = partialBlockName $ fromJust mbb

    br retB -- here is the problem... we may have generated intermediate blocks so we need to get the correct label
    pure ((tag, block), (val, label))

compileBody a@(App _ _) = do
  let (Var f, args) = extractTyApp a []
      unwrapped = extractApp a []
      vars = varsInType (idTy f) \\ freeVariables (idTy f)
      appliedType = replaceTypeVars (zip vars args) (idTy f)
      appRetTy = llvmArgType . last $ unwrapN (length $ snd unwrapped) appliedType
  c <- compileCall unwrapped
  bitcast c appRetTy
compileBody exp = mkInt =<< int64 (-97)

extractTyApp :: Core Var -> [Type Name] -> (Core Var, [Type Name])
extractTyApp (App f (Type t)) acc = extractTyApp f (t : acc)
extractTyApp (App f e)        acc = extractTyApp f (acc)
extractTyApp e acc = (e, acc)

extractApp :: Core Var -> [CoreExp] -> (Core Var, [CoreExp])
extractApp (App f (Type _)) acc = extractApp f (acc)
extractApp (App f e)        acc = extractApp f (e : acc)
extractApp e acc = (e, acc)

compileCall (c, []) = compileBody c
compileCall (Var func, args) = do
  globalDefs <- reader globalInfo
  case varName func `lookup` globalDefs of
    Just i@(Info arity _ _ _)
      | arity == length args -> do
        knownCall i func args
      | arity < length args -> do
        let (args', ukArgs) = splitAt arity args

        closePtr <- knownCall i func args'
        cArgs <- mapM compileBody ukArgs
        let retTy = llvmArgType $ last $ unwrapN (length args) (idTy func)
        closurePtr <- unknownCall closePtr cArgs

        bitcast closurePtr retTy

      | arity >  length args -> do
        op <- buildClosure i func
        args <- mapM compileBody args

        closurePtr <- unknownCall op args
        bitcast closurePtr closureType
    Nothing -> do
      closePtr <- compileBody (Var func)

      args <- mapM compileBody args
      let retTy = llvmArgType $ last $ unwrapN (length args) (idTy func)

      closurePtr <- unknownCall closePtr args
      bitcast closurePtr retTy

knownCall (Info arity argTys _ _) i@(Id{}) args = do
  args' <- mapM compileBody args
  dict <- reader globalInfo

  let f' = operand . fromJust $ varName i `lookup` dict

  args'' <- forM (zip args' argTys) $ \(arg, ty) -> do
    if T.typeOf arg == ty
    then pure arg
    else bitcast arg ty

  call f' (map (\arg -> (arg, [])) args'')

unknownCall closurePtr args = do
  let (T.PointerType (T.StructureType _ ls) _) = T.typeOf closurePtr

  let fTy = ptr $ T.FunctionType (ptr T.i8) [closureType, ptr $ T.i8] False
  foldM (\clos arg -> do
    clos' <- if T.typeOf clos == ptr T.i8
      then bitcast clos closureType
      else pure clos
    arg' <- bitcast arg (ptr $ T.i8)
    call (AST.ConstantOperand $ C.GlobalReference fTy (fromString "apply1")) [(clos', []), (arg', [])]
    ) closurePtr args

{-
  Closure Representation

  After a lot of head scratching, I've settled on this fairly naive approach to closure representation.

  +------------------+-----------------+------+--------+-----+
  | Function Pointer | Remaining Arity | ArgN | ArgN-1 | ... |
  +------------------+-----------------+------+--------+-----+

  Note, the arguments are stored in reverse order. The reason for this is that it allows us to use the
  remaining arity to directly index the closure. If we stored them in forward order, we'd also need to
  store the fully arity in order to determine the offset for the next argument we wish to store.
-}

sizeofType ty = ConstantOperand $ C.PtrToInt (C.GetElementPtr False (C.Null ty) [C.Int 32 1]) T.i64

buildClosure :: CodegenM m => BindingInfo -> Var -> m Operand
buildClosure (Info _ argTys retTy _) i@(Id{}) = do
  voidPtr <- malloc $ sizeofType closureType
  memPtr <- bitcast voidPtr closureType
  val <- load memPtr 8
  let name = AST.ConstantOperand $ C.GlobalReference closureFuncTy (fromString $ "callClosure" ++ varName i)

  voidPtr <- bitcast name (ptr $ T.i8)
  val' <- insertvalue val voidPtr [0]
  ix <- byte . fromIntegral $ length argTys
  val'' <- insertvalue val' ix [1]

  store memPtr 8 val''
  return memPtr
  where
  closureFuncTy = ptr $ T.FunctionType retTy [closureType] False

mkClosureCall :: ModuleM m => Int -> Var -> m Operand
mkClosureCall arity i@(Id{}) = function (fromString $ "callClosure" ++ varName i) [(closureType, "closure")] llvmRetTy $ \[closure] -> mdo
  block `named` "entry" ; do
    args <- forM (zip [1..] llvmArgTy') $ \(i, ty) -> do
      argPtr <- gep closure $ int32 0 ++ int32 2 ++ int32 i
      ptr' <- bitcast argPtr (ptr ty)
      load ptr' 8

    let func = AST.ConstantOperand $ C.GlobalReference (ptr $ T.FunctionType llvmRetTy (reverse llvmArgTy') False) (fromString $ varName i)
    retCall <- call func (map (\arg -> (arg, [])) $ reverse args)

    ret retCall
  where
  llvmArgTy' = reverse $ map llvmArgType (init unwrapped)
  llvmRetTy  = llvmArgType (last unwrapped)
  unwrapped = unwrapN arity (idTy i)
