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
  { vars :: [(Id, AST.Operand)]
  , arities :: [(Id, Int)]
  } deriving (Show, Eq)

updateVars f (CS v a) = CS (f v) a
updateArities f (CS v a) = CS v (f a)

compileModule :: CoreModule -> AST.Module
compileModule mod =  buildModule "example" . (flip runReaderT (CS nameMap arityMap)) $ mdo
  extern "malloc" [T.NamedTypeReference "size_t"] (ptr T.void)

  defMkDouble
  defMkInt

  forM (constructors mod) compileConstructor
  forM (filter isLambda $ bindings mod) compileBinding
  where
  isLambda (NonRec _ Lambda{}) = True
  isLambda _ = False

  nameMap = map nameTuple (bindings mod) ++ map (\(nm, (_, t)) -> (nm, ConstantOperand $ C.GlobalReference (typeToLlvmType' t) (fromString nm))) (constructors mod)
  nameTuple (NonRec v _) = (varName v, ConstantOperand $ C.GlobalReference (typeToLlvmType' (idTy v)) (fromString $ varName v))
  arityMap = map arityTuple (bindings mod) ++ map (\(nm, (i, _)) -> (nm, i)) (constructors mod)
  arityTuple (NonRec v b) = (varName v, length . snd $ unwrapLambda b)

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
  llvmArgTy' = map typeToLlvmType argTys
  argTys = init (unwrapFnType ty)
  funArgs = zip llvmArgTy' (repeat (fromString "a"))

ptr x = T.PointerType x (AddrSpace 0)

typeToLlvmType = ptr . typeToLlvmType'
typeToLlvmType' (TVar nm) = T.NamedTypeReference (fromString nm)
typeToLlvmType' f@(Arrow _ _) = T.FunctionType (typeToLlvmType (last tys)) (map typeToLlvmType (init tys)) False
  where tys = unwrapFnType f
typeToLlvmType' f@(TAp (TAp (TConstructor "->") a) b) = T.FunctionType (typeToLlvmType (last tys)) (map typeToLlvmType (init tys)) False
  where tys = unwrapFnType f
typeToLlvmType' (TConstructor nm) = T.NamedTypeReference (fromString nm)
typeToLlvmType' ap@(TAp _ _) = let
  cons : _ = unwrapProduct ap
  in typeToLlvmType' cons
typeToLlvmType' (Forall _ t) = typeToLlvmType' t
typeToLlvmType' t = error $ show t

type CodegenM m = (MonadFix m, MonadIRBuilder m, MonadReader CodegenState m)
type ModuleM  m = (MonadFix m, MonadModuleBuilder m, MonadReader CodegenState m)

compileBinding :: ModuleM m => Bind Var -> m ()
compileBinding (NonRec nm l@(Lambda _ _)) = do
  when (length argVars > 0) $ void $ mkClosureCall nm

  function (fromString $ varName nm) args retTy $ \args -> mdo
    block `named` "entry" ; do
      let dict = zipWith (\v op -> (fromString $ varName v, op)) argVars args
      local (updateVars (dict ++)) $ compileBody body >>= ret

  pure ()

  where
  args = map (\var -> (typeToLlvmType (idTy var), fromString $ varName var)) argVars
  retTy = typeToLlvmType $ last $ unwrapFnType $ idTy nm
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
  dict <- reader vars
  pure . nameAssert $ varName v `lookup` dict
  where
  nameAssert (Just nm) = nm
  nameAssert Nothing = error $ "no " ++ varName v ++ " found."
compileBody (Let (NonRec v e) exp) = do -- figure out how to handle recursive let bindings
  cE <- compileBody e

  local (updateVars ((varName v, cE) :)) $ compileBody exp
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

    local (updateVars (bindDict ++)) $ compileBody b
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
  globalDefs <- reader arities

  case varName func `lookup` globalDefs of
    Just arity
      | arity == length args -> knownCall arity func args
      | arity <  length args -> mkInt =<< int64 (-98) -- take first arity args and then do uknnown call
      | arity >  length args -> do
        op <- buildClosure func
        args <- mapM compileBody args

        unknownCall op args

        mkInt =<< int64 (-99) -- mkClosure arity func args
    Nothing -> mkInt =<< int64 (-95) -- unknownCall func args

knownCall arity i@(Id{}) args = do
  args' <- mapM compileBody args
  f' <- compileBody (Var i)
  call f' (map (\arg -> (arg, [])) args')

unknownCall closure args = do
  traceShowM (T.typeOf closure)
  let (T.PointerType (T.StructureType _ ls) _) = T.typeOf closure

  forM (zip args $ reverse [1..length ls]) $ \(args, ix) -> do
    insertvalue closure args [fromIntegral ix]
  unreachable

buildClosure :: CodegenM m => Var -> m Operand
buildClosure i@(Id{}) = do
  voidPtr <- malloc (ConstantOperand consSize)
  memPtr <- bitcast voidPtr (closureTy)
  val <- load memPtr 8
  let name = AST.ConstantOperand $ C.GlobalReference closureFuncTy (fromString $ "callClosure" ++ varName i)
  insertvalue val name [0]
  return memPtr
  where
  consSize = C.GetElementPtr False (C.Null $ closureTy) [C.Int 32 1]

  closureFuncTy = T.FunctionType llvmRetTy [closureTy] False
  closureTy  = ptr $ T.StructureType False (typeToLlvmType (idTy i) : llvmArgTy')
  llvmArgTy' = map typeToLlvmType argTys
  llvmRetTy  = typeToLlvmType retTy
  retTy  = last unwrapped
  argTys = init unwrapped
  unwrapped = unwrapFnType (idTy i)

mkClosureCall i@(Id{}) = function (fromString $ "callClosure" ++ varName i) [(closureTy, "closure")] llvmRetTy $ \[closure] -> mdo
  block `named` "entry" ; do
    args <- forM (zip [1..] llvmArgTy') $ \(i, _) -> do
      gep closure $ int32 i

    func <- gep closure (int32 0 ++ int32 0 ++ int32 0)

    traceShowM (T.typeOf closure)
    traceShowM (idTy i)
    retCall <- call func (map (\arg -> (arg, [])) $ reverse args)

    ret retCall
  where
  closureTy  = ptr $ T.StructureType False (typeToLlvmType (idTy i) : llvmArgTy')
  llvmArgTy' = map typeToLlvmType argTys
  llvmRetTy  = typeToLlvmType retTy
  retTy  = last unwrapped
  argTys = init unwrapped
  unwrapped = unwrapFnType (idTy i)
