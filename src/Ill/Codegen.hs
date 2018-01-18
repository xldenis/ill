{-# LANGUAGE RecursiveDo, OverloadedStrings, ConstraintKinds #-}
module Ill.Codegen where

import Control.Monad (forM)
import Data.String

import           Ill.Syntax.Core
import           Ill.Syntax.Literal
import           Ill.Syntax.Name
import           Ill.Syntax.Type

import           LLVM.AST.AddrSpace
import           LLVM.IRBuilder
import           LLVM.IRBuilder.Instruction
import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Monad
import           LLVM.Pretty
import           LLVM.AST.Operand
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Type as T
import qualified LLVM.AST.Typed as T
import qualified Control.Monad as M

import           Ill.Prelude hiding (void)
import           Data.Word
import           Data.Maybe
import           Data.List (find)
import           Control.Monad.Fix
import           Control.Monad.Reader

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

malloc :: MonadIRBuilder m => AST.Operand -> m AST.Operand
malloc size = call (AST.LocalReference mallocTy "malloc") [(size, [])]
  where mallocTy = T.FunctionType (ptr T.void) [T.i32] False

data CodegenState = CS
  { globalInfo :: [(Id, BindingInfo)]
  , localInfo :: [(Id, AST.Operand)]
  } deriving (Show, Eq)

updateLocals f (CS c l) = CS c (f l)

compileModule :: CoreModule -> AST.Module
compileModule mod =  buildModule "example" . (flip runReaderT (CS infoMap [])) $ mdo
  extern "malloc" [T.NamedTypeReference "size_t"] (ptr T.void)
  extern "gtInt" [ptr $ T.NamedTypeReference "Int", ptr $ T.NamedTypeReference "Int"] (ptr $ T.NamedTypeReference "Bool")
  defMkDouble
  defMkInt

  forM (constructors mod) compileConstructor
  forM (filter isLambda $ bindings mod) compileBinding
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
    op  = ConstantOperand $ C.GlobalReference (T.FunctionType ret args False) (fromString nm)
    in (nm, Info arity args ret op)

collectConstructorInfo :: (Name, (Int, Type Name)) -> (Id, BindingInfo)
collectConstructorInfo (nm, (arity, ty)) = let
  tys = unwrapFnType ty
  args = map llvmArgType $ init tys
  ret  = llvmArgType $ last tys

  op = ConstantOperand $ C.GlobalReference (T.FunctionType ret args False) (fromString nm)

  in (nm, Info arity args ret op)

collectBindingInfo :: Bind Var -> (Id, BindingInfo)
collectBindingInfo (NonRec v b) = let
  arity = length . snd $ unwrapLambda b
  tys   = unwrapN arity (idTy v)
  args  = map llvmArgType $ init tys
  ret   = llvmArgType $ last tys
  op    = ConstantOperand $ C.GlobalReference (T.FunctionType ret args False) (fromString $ varName v)
  in (varName v, Info arity args ret op)

defMkDouble = function "mkDouble" [(T.double, "d")] (ptr T.void) $ \[d] -> do
  block `named` "entry" ; do
    memPtr <- malloc =<< (int64 $ 8 + 8)
    store memPtr 8 d
    ret memPtr

mkDouble d = call (AST.LocalReference mkDoubleTy "mkDouble") [(d, [])]
  where mkDoubleTy = T.FunctionType (ptr T.double) [T.double] False

defMkInt = function "mkInt" [(T.i64, "d")] (ptr $ T.NamedTypeReference (fromString "Int")) $ \[d] -> do
  block `named` "entry" ; do
    memPtr <- malloc =<< (int64 $ 8 + 8)
    store memPtr 8 d
    ret memPtr

mkInt d = call (AST.ConstantOperand $ C.GlobalReference mkIntTy "mkInt") [(d, [])]
  where mkIntTy = T.FunctionType (ptr $ T.NamedTypeReference (fromString "Int")) [T.i64] False

compileConstructor :: MonadModuleBuilder m => (Name, (Int, Type Name)) -> m ()
compileConstructor (nm, (_, ty)) = do
  typedef (fromString nm) (Just $ T.StructureType False llvmArgTys)

  M.void $ function (fromString $ nm) funArgs (ptr consTy) $ \args -> do
    block `named` "entryC" ; do
      voidPtr <- malloc (ConstantOperand consSize)
      memPtr <- bitcast voidPtr (ptr consTy)
      val <- load memPtr 8
      header <- int64 0
      headerVal <- insertvalue val header [0]
      built <- M.foldM (\prev (ix, arg) -> insertvalue prev arg [ix]) val (zip [1..] args)

      store memPtr 8 built
      ret memPtr
  where
  consTy = T.NamedTypeReference $ fromString nm
  consSize = C.GetElementPtr False (C.Null $ ptr $ consTy) [C.Int 32 1]
  llvmArgTys = T.i64 : llvmArgTy'
  llvmArgTy' = map llvmArgType argTys
  argTys = init (unwrapFnType ty)
  funArgs = zip llvmArgTy' (repeat (fromString "a"))

ptr x = T.PointerType x (AddrSpace 0)

typeToLlvmType = ptr . typeToLlvmType'
typeToLlvmType' (TVar nm) = T.NamedTypeReference (fromString nm)
typeToLlvmType' f@(Arrow _ _) =
  T.FunctionType llvmRetTy llvmArgTy' False
  where
  llvmArgTy' = map llvmArgType argTys
  llvmRetTy  = llvmArgType retTy
  retTy  = last unwrapped
  argTys = init unwrapped
  unwrapped = unwrapFnType f
typeToLlvmType' f@(TAp (TAp (TConstructor "->") a) b) =
  T.FunctionType llvmRetTy llvmArgTy' False
  where
  llvmArgTy' = map llvmArgType argTys
  llvmRetTy  = llvmArgType retTy
  retTy  = last unwrapped
  argTys = init unwrapped
  unwrapped = unwrapFnType f
typeToLlvmType' (TConstructor nm) = T.NamedTypeReference (fromString nm)
typeToLlvmType' ap@(TAp _ _) = let
  cons : _ = unwrapProduct ap
  in typeToLlvmType' cons
typeToLlvmType' (Forall _ t) = typeToLlvmType' t
typeToLlvmType' t = error $ show t

llvmArgType (TVar nm) = ptr $ T.NamedTypeReference (fromString nm)
llvmArgType (TConstructor nm) = ptr $ T.NamedTypeReference (fromString nm)
llvmArgType ap@(TAp _ _) = let
  cons : _ = unwrapProduct ap
  in llvmArgType cons
llvmArgType (Forall _ t) = llvmArgType t
llvmArgType f@(Arrow _ _) =
  ptr $ T.StructureType False (ptr T.void : llvmArgTy')
  where
  llvmArgTy' = map llvmArgType argTys
  llvmRetTy  = llvmArgType retTy
  retTy  = last unwrapped
  argTys = init unwrapped
  unwrapped = unwrapFnType f
llvmArgType f@(TAp (TAp (TConstructor "->") a) b) =
  ptr $ T.StructureType False (ptr T.void : llvmArgTy')
  where
  llvmArgTy' = map llvmArgType argTys
  llvmRetTy  = llvmArgType retTy
  retTy  = last unwrapped
  argTys = init unwrapped
  unwrapped = unwrapFnType f
llvmArgType t = error $ show t

type CodegenM m = (MonadFix m, MonadIRBuilder m,     MonadReader CodegenState m)
type ModuleM  m = (MonadFix m, MonadModuleBuilder m, MonadReader CodegenState m)

compileBinding :: ModuleM m => Bind Var -> m ()
compileBinding (NonRec nm l@(Lambda _ _)) = do
  when (length argVars > 0) $ void $ mkClosureCall nm

  function (fromString $ varName nm) args retTy $ \args -> mdo
    block `named` "entry" ; do
      let dict = zipWith (\v op -> (fromString $ varName v, op)) argVars args
      local (updateLocals (dict ++)) $ compileBody body >>= ret

  pure ()

  where
  args = map (\var -> (llvmArgType (idTy var), fromString $ varName var)) argVars
  retTy = llvmArgType $ last $ unwrapFnType $ idTy nm
  (body, argVars) = unwrapLambda l

unwrapLambda :: Core Var -> (Core Var, [Var])
unwrapLambda (Lambda b@(Id{}) e) = (b :) <$> unwrapLambda e
unwrapLambda (Lambda _ e) = unwrapLambda e
unwrapLambda e = (e, [])
-- compileBinding exp = unreachable -- error $ show exp

compileBody :: CodegenM m => CoreExp -> m AST.Operand
compileBody (Lit (Double d)) = mkDouble =<< double d -- BOX IT
compileBody (Lit (Integer i)) = mkInt =<< int64 i
compileBody (Var v) = do
  dict <- reader localInfo

  globalDefs <- reader globalInfo

  case varName v `lookup` globalDefs of
    Just _ -> buildClosure v
    Nothing ->
      pure . nameAssert $ varName v `lookup` dict
  where
  nameAssert (Just nm) = nm
  nameAssert Nothing = error $ "no " ++ varName v ++ " found."
compileBody (Let (NonRec v e) exp) = do -- figure out how to handle recursive let bindings
  cE <- compileBody e

  local (updateLocals ((varName v, cE) :)) $ compileBody exp
compileBody (Case scrut alts) = mdo
  scrutOp <- compileBody scrut
  val <- load scrutOp 8
  tag <- extractvalue (T.i64) val [0]

  switch tag defAlt alts'

  (alts', phis) <- unzip <$> M.zipWithM (compileAlt retBlock val) [1..] (filter isConAlt alts)
  defAlt <- fromJust $ (compileDefaultAlt retBlock) <$> (find isTrivialAlt alts) <|> pure (defaultBranch retBlock)

  retBlock <- block `named` "switch_return" ; do
    phi phis
  where
  fromJust' (Just x) = x
  fromJust' _ = error (show alts)

  defaultBranch retB = do
    blk <- block `named` "default_branch" ; do
      br retB
    pure blk

  compileDefaultAlt retB (TrivialAlt b) = do
    blk <- block `named` "trivial_branch" ; do
      compileBody b
      br retB
    pure blk
  compileAlt retB scrut i (ConAlt cons binds b) = do
    block <- block `named` "branch"
    scrut' <- bitcast scrut $ T.NamedTypeReference (fromString cons)
    bindVals <- catMaybes <$> (forM (zip [1..] binds) $ \(ix, v) -> do
      case usage v of
        Used -> Just <$> extractvalue (typeToLlvmType (idTy v)) scrut' [ix]
        NotUsed -> pure Nothing)

    let bindDict = zipWith (\var val -> (fromString $ varName var, val)) binds bindVals
    let tag = C.Int 64 i

    local (updateLocals (bindDict ++)) $ compileBody b
    val <- int64 5
    br retB
    pure ((tag, block), (val, block))

compileBody a@(App _ _) = compileCall (unwrapApp a [])
compileBody exp = mkInt =<< int64 (-97)

unwrapApp :: Core Var -> [CoreExp] -> (Core Var, [CoreExp])
unwrapApp (App f (Type _)) acc = unwrapApp f (acc)
unwrapApp (App f e)        acc = unwrapApp f (e : acc)
unwrapApp e acc = (e, acc)

compileCall (c, []) = compileBody c
compileCall (Var func, args) = do
  globalDefs <- reader globalInfo

  case varName func `lookup` globalDefs of
    Just (Info arity _ _ _)
      | arity == length args -> knownCall arity func args
      | arity <  length args -> do
        let (args', ukArgs) = splitAt arity args

        closePtr <- knownCall arity func args'
        cArgs <- mapM compileBody args

        unknownCall closePtr cArgs
      | arity >  length args -> do
        op <- buildClosure func
        args <- mapM compileBody args

        unknownCall op args
    Nothing -> do
      closePtr <- compileBody (Var func)

      args <- mapM compileBody args

      unknownCall closePtr args

knownCall arity i@(Id{}) args = do
  args' <- mapM compileBody args
  dict <- reader globalInfo

  let f' = operand . fromJust $ varName i `lookup` dict
  call f' (map (\arg -> (arg, [])) args')

unknownCall closurePtr args = do
  let (T.PointerType (T.StructureType _ ls) _) = T.typeOf closurePtr

  closure <- load closurePtr 8

  updated <- foldM (\closure (args, ix) -> do
    insertvalue closure args [fromIntegral ix]
    ) closure (zip args $ reverse [1..length ls])

  store closurePtr 8 updated

  if (length ls == length args) then do
    voidPtr <- extractvalue (ptr T.void) closure ([0])
    fPtr <- bitcast voidPtr (ptr $ T.FunctionType (ptr $ T.void) [T.typeOf closurePtr] False)
    call fPtr [(closurePtr, [])]
  else pure closurePtr

buildClosure :: CodegenM m => Var -> m Operand
buildClosure i@(Id{}) = do
  voidPtr <- malloc (ConstantOperand consSize)
  memPtr <- bitcast voidPtr closureTy
  val <- load memPtr 8
  let name = AST.ConstantOperand $ C.GlobalReference closureFuncTy (fromString $ "callClosure" ++ varName i)
  insertvalue val name [0]
  return memPtr
  where
  consSize = C.GetElementPtr False (C.Null $ closureTy) [C.Int 32 1]

  closureFuncTy = T.FunctionType llvmRetTy [closureTy] False
  closureTy  = ptr $ T.StructureType False (ptr T.void : llvmArgTy')
  llvmArgTy' = map llvmArgType argTys
  llvmRetTy  = llvmArgType retTy
  retTy  = last unwrapped
  argTys = init unwrapped
  unwrapped = unwrapFnType (idTy i)

mkClosureCall i@(Id{}) = function (fromString $ "callClosure" ++ varName i) [(closureTy, "closure")] llvmRetTy $ \[closure] -> mdo
  block `named` "entry" ; do
    args <- forM (zip [1..] llvmArgTy') $ \(i, _) -> do
      ptr <- gep closure $ int32 0 ++ int32 i
      load ptr 8

    let func = AST.ConstantOperand $ C.GlobalReference (T.FunctionType llvmRetTy llvmArgTy' False) (fromString $ varName i)
    retCall <- call func (map (\arg -> (arg, [])) $ reverse args)

    ret retCall
  where

  closureFuncTy = T.FunctionType llvmRetTy [closureTy] False
  closureTy  = ptr $ T.StructureType False (ptr T.void : llvmArgTy')
  llvmArgTy' = map llvmArgType argTys
  llvmRetTy  = llvmArgType retTy
  retTy  = last unwrapped
  argTys = init unwrapped
  unwrapped = unwrapFnType (idTy i)
