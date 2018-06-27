{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ill.Codegen where

import           Control.Monad             (forM)
import           Control.Monad.Fix
import           Control.Monad.Reader
import           Control.Monad.State       (gets)
import qualified Control.Monad             as M

import           Data.Char
import           Data.List                 (find)
import           Data.Map                  ((!))
import           Data.Maybe
import           Data.String
import           Data.Text                 (pack)
import           Data.Word
import qualified Data.ByteString           as BS
import qualified Data.Text.Encoding        as Encoding

import           Ill.Codegen.Closure
import           Ill.Codegen.Monad
import           Ill.Prelude               hiding (void)
import           Ill.Syntax.Core
import           Ill.Syntax.Literal
import           Ill.Syntax.Name
import           Ill.Syntax.Pretty
import           Ill.Syntax.Type
import qualified Ill.Syntax.Builtins       as Builtins

import           LLVM.AST.AddrSpace
import           LLVM.AST.Operand (Operand(..))
import           LLVM.IRBuilder
import           LLVM.Pretty
import qualified LLVM.AST                  as AST
import qualified LLVM.AST.Constant         as C
import qualified LLVM.AST.IntegerPredicate as AST
import qualified LLVM.AST.Type             as T
import qualified LLVM.AST.Typed            as T

{-
  init doesnt work
-}
prettyModule mod =  ppllvm $ compileModule mod

compileModule :: CoreModule -> AST.Module
compileModule mod =  buildModule "example" . flip runReaderT (fromModule mod) $ mdo
  declareBuiltins

  forM (types mod) $ \nm -> typedef (fromString $ qualName nm) $ Just $ T.StructureType False [T.i64]
  forM (constructors mod) compileConstructor
  forM (bindings mod) compileBinding

  where

  isLambda (NonRec _ Lambda{}) = True
  isLambda _                   = False

  builtinExtern (nm, ty) = extern (fromString $ qualName nm) (map llvmArgType args) (llvmArgType ret)
    where
    tys = unwrapFnType ty
    args = init tys
    ret  = last tys

  declareBuiltins = do
    extern "GC_malloc"   [T.i64] (ptr T.i8)
    extern "memcpy"   [ptr T.i8, ptr T.i8, T.i64] (ptr T.i8)

    forM Builtins.primitives builtinExtern

    typedef "String" (Just $ T.StructureType False [T.i64, ptr T.i8])
    typedef "Char"   (Just $ T.StructureType False [T.i32])

    defMkDouble
    defMkInt
    apply1

defMkDouble :: ModuleM m => m Operand
defMkDouble = do
  typedef (fromString "Double") (Just $ T.StructureType False [T.double])

  functionWithGlobals functionInlineable "mkDouble" [(T.double, "d")] primDouble $ \[d] -> do
    block `named` "entry" ; do
      memPtr <- malloc $ sizeofType primDouble
      iPtr <- bitcast memPtr primDouble
      i <- load iPtr 8
      i' <- insertvalue i d [0]
      store iPtr 8 i'
      ret iPtr

mkDouble d = call (AST.LocalReference mkDoubleTy "mkDouble") [(d, [])]
  where mkDoubleTy = ptr $ T.FunctionType primDouble [T.double] False

defMkInt :: ModuleM m => m Operand
defMkInt = do
  typedef (fromString "Int") (Just $ T.StructureType False [T.i64])

  functionWithGlobals functionInlineable "mkInt" [(T.i64, "d")] primInt $ \[d] -> do
    block `named` "entry" ; do
      memPtr <- malloc (sizeofType primInt)
      iPtr <- bitcast memPtr primInt
      i <- load iPtr 8
      i' <- insertvalue i d [0]
      store iPtr 8 i'
      ret iPtr

mkInt d = call (AST.ConstantOperand $ C.GlobalReference mkIntTy "mkInt") [(d, [])]
  where mkIntTy = ptr $ T.FunctionType primInt [T.i64] False

mkString str = do
  let charOrds = BS.unpack (Encoding.encodeUtf8 $ pack str) ++ [0]
  let chars = map (C.Int 8 . fromIntegral) charOrds

  arr <- array chars

  voidPtr <- malloc =<< (int64 . fromIntegral $ length charOrds)
  arrPtr  <- bitcast voidPtr (ptr $ T.typeOf arr)
  store arrPtr 8 arr

  str <- struct Nothing False [C.Int 64 . fromIntegral $ length str, C.Null (ptr $ T.i8)]
  charPtr <- bitcast arrPtr (ptr T.i8)
  fullStruct <- insertvalue str charPtr [1]

  voidPtr <- malloc (sizeofType . ptr $ T.typeOf fullStruct)
  memPtr <- bitcast voidPtr (ptr $ T.typeOf fullStruct)
  store memPtr 8 fullStruct

  bitcast memPtr (ptr $ T.NamedTypeReference "String")


compileConstructor :: MonadModuleBuilder m => (QualifiedName, ConstructorEntry) -> m ()
compileConstructor (nm, cons) = do
  typedef (fromString $ qualName nm) (Just $ T.StructureType False llvmArgTys)

  M.void $ function (fromString $ qualName nm) funArgs retTy $ \args -> do
    block `named` "entryC" ; do
      voidPtr <- malloc (sizeofType $ ptr consTy)
      memPtr <- bitcast voidPtr (ptr consTy)
      val <- load memPtr 8

      header <- int64 (fromIntegral $ consTag cons)
      headerVal <- insertvalue val header [0]
      built <- M.foldM (\prev (ix, arg) -> insertvalue prev arg [ix]) headerVal (zip [1..] args)

      store memPtr 8 built

      retPtr <- bitcast memPtr retTy
      ret retPtr
  where
  consTy = T.NamedTypeReference $ fromString $ qualName nm
  llvmArgTys = T.i64 : llvmArgTy'
  llvmArgTy' = map llvmArgType argTys
  argTys = init unwrapped
  unwrapped = unwrapFnType (consType cons)
  retTy = llvmArgType (last unwrapped)
  funArgs = zip llvmArgTy' (repeat (fromString "a"))

compileBinding :: ModuleM m => Bind Var -> m ()
compileBinding (NonRec v l) | qualName (varName v) == "main" = compileBinding (NonRec (v { varName = fmap (const "module_main") (varName v) }) l)
compileBinding (NonRec nm l) = do
  when (not (null argVars)) $ void $ mkClosureCall (length argVars) nm

  function (fromString . qualName $ varName nm) args retTy $ \args -> mdo
    block `named` "entry" ; do
      let dict = zipWith (\v op -> (varName v, op)) argVars args
      local (updateLocals (dict ++)) $ do
        retVal <- compileBody body
        bitcast retVal retTy >>= ret

  pure ()

  where
  args = map (\var -> (llvmArgType (idTy var), fromString . qualName $ varName var)) argVars
  retTy = llvmArgType $ last $ unwrapN (length argVars) $ idTy nm
  (body, argVars) = unwrapLambda l

compileBody :: forall m . CodegenM m => CoreExp -> m AST.Operand
compileBody (Lit (Double d)) = mkDouble =<< double d
compileBody (Lit (Integer i)) = mkInt =<< int64 i
compileBody (Lit (RawString i)) = mkString i
compileBody (Var v) = do
  dict <- reader localInfo
  globalDefs <- reader globalInfo

  case varName v `lookup` globalDefs of
    Just i | arity i == 0 -> knownCall i v []
    Just i -> buildClosure i v
    Nothing ->
      pure . nameAssert dict $ varName v `lookup` dict
  where
  nameAssert _ (Just nm)  = nm
  nameAssert dict Nothing = error $ "no " ++ show v ++ show dict ++ " found."
compileBody l@(Let (NonRec v e) exp) = do -- figure out how to handle recursive let bindings
  cE <- compileBody e

  local (updateLocals ((varName v, cE) :)) $ compileBody exp
compileBody (Case scrut alts) = mdo
  scrutOp <- compileBody scrut

  let (tagIx, scrutTy) = (0, ptr $ T.StructureType False [T.i64])

  scrutHead <- bitcast scrutOp scrutTy
  tagPtr <- gep scrutHead (int32 0 ++ int32 tagIx)
  tag <- load tagPtr 8

  switch tag defAlt alts'

  (alts', phis) <- unzip <$> M.mapM (compileAlt retBlock scrutOp) (filter (not . isTrivialAlt) alts)
  (defAlt, maybeBody) <- fromJust' $ compileDefaultAlt retBlock <$> find isTrivialAlt alts <|> pure (defaultBranch retBlock)

  retBlock <- block `named` "switch_return" ;
    phi $ maybeToList maybeBody ++ phis
  where
  fromJust' (Just x) = x
  fromJust' _        = error (show alts)

  defaultBranch :: AST.Name -> m (AST.Name, Maybe a)
  defaultBranch retB = do
    blk <- block `named` "default_branch" ;
      unreachable
    pure (blk, Nothing)

  compileDefaultAlt :: AST.Name -> Alt Var -> m (AST.Name, Maybe (Operand, AST.Name))
  compileDefaultAlt retB (TrivialAlt (App (Var v) _)) | qualName (varName v) == "failedPattern" = do
    blk <- block `named` "trivial_branch" ;
      unreachable
    pure (blk, Nothing)
  compileDefaultAlt retB (TrivialAlt b) = do
    blk <- block `named` "trivial_branch" ; do
      body <- compileBody b
      br retB

      mbb <- liftIRState $ gets builderBlock

      let label = partialBlockName $ fromJust' mbb
      pure (blk, Just (body, label))

  compileAlt :: AST.Name -> Operand -> Alt Var -> m ((C.Constant, AST.Name), (Operand, AST.Name))
  compileAlt retB scrut c@(ConAlt cons binds b) = do
    block <- block `named` fromString ("branch " ++ (qualName cons))
    scrutPtr <- bitcast scrut $ ptr $ T.NamedTypeReference (fromString $ qualName cons)
    scrut' <- load scrutPtr 8

    (Info _ argTys _ _) <- reader (\s -> fromJust' $ cons `lookup` globalInfo s)

    bindVals <- catMaybes <$> forM (zip3 [1..] binds argTys) (\(ix, v, ty) -> do
      case usage v of
        Used -> do
          field <- extractvalue ty scrut' [ix]
          return $ Just (varName v, field)
        NotUsed -> pure Nothing)

    consInfo <- reader (fromJust . lookup cons . consInfo)

    let tag = C.Int 64 (fromIntegral $ consTag consInfo)
    val <- local (updateLocals (bindVals ++)) $ compileBody b
    mbb <- liftIRState $ gets builderBlock

    let label = partialBlockName $ fromJust' mbb

    br retB
    pure ((tag, block), (val, label))
  compileAlt retB _ c@(LitAlt (Integer i) b) = do
    block <- block `named` fromString ("branch " ++ show i)

    let tag = C.Int 64 (fromIntegral i)

    val <- compileBody b
    mbb <- liftIRState $ gets builderBlock
    let label = partialBlockName $ fromJust' mbb

    br retB

    pure ((tag, block), (val, label))

compileBody a@(App _ _) = do
  let (Var f, (tys, args)) = extractApp a
      vars = varsInType (idTy f) \\ freeVariables (idTy f)
      appliedType = replaceTypeVars (zip vars tys) (idTy f)
      appRetTy = llvmArgType . last $ unwrapN (length args) appliedType
  c <- compileCall (Var f) args
  bitcast c appRetTy
compileBody exp = mkInt =<< int64 (-97)

extractApp :: Core Var -> (Core Var, ([Type QualifiedName], [CoreExp]))
extractApp core = go core ([], [])
  where
  go (App f (Type t)) (tys, exps) = go f (t : tys, exps)
  go (App f e)        (tys, exps) = go f (tys, e : exps)
  go e acc                        = (e, acc)

{-
  Function calls and closures

  The following methods handle compiling function applications.

  First an analysis to determine the kind of function call required needs to be performed.

  - If the call has less arguments than the arity of the function, we build a closure holding the
  partially applied arguments.

  - If it has exactly the same amount of arguments as it's arity we can generate a direct function
  call.

  - If it has _too_ many arguments, we take the first `arity` arguments, generate a direct function
  call, then partially apply the remaining ones to the first of the first call.

-}
compileCall :: CodegenM m => CoreExp -> [CoreExp] -> m Operand
compileCall c [] = compileBody c
compileCall (Var func) args = do
  globalDefs <- reader globalInfo
  case varName func `lookup` globalDefs of
    Just i@(Info arity _ _ _)
      | arity == length args ->
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

knownCall :: CodegenM m => BindingInfo -> Var -> [CoreExp] -> m Operand
knownCall (Info arity argTys _ _) i@Id{} args = do
  args' <- mapM compileBody args
  dict <- reader globalInfo

  let f' = operand . fromJust' $ varName i `lookup` dict

  args'' <- forM (zip args' argTys) $ \(arg, ty) ->
    if T.typeOf arg == ty
    then pure arg
    else bitcast arg ty

  call f' (map (\arg -> (arg, [])) args'')
  where
  fromJust' (Just x) = x
  fromJust' _        = error "Known call could not be found."

unknownCall :: MonadIRBuilder m => Operand -> [Operand] -> m Operand
unknownCall closurePtr args = do
  let (T.PointerType (T.StructureType _ ls) _) = T.typeOf closurePtr

  let fTy = ptr $ T.FunctionType (ptr T.i8) [closureType, ptr T.i8] False
  foldM (\clos arg -> do
    clos' <- if T.typeOf clos == ptr T.i8
      then bitcast clos closureType
      else pure clos
    arg' <- bitcast arg (ptr T.i8)
    call (AST.ConstantOperand $ C.GlobalReference fTy (fromString "apply1")) [(clos', []), (arg', [])]
    ) closurePtr args
