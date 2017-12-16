{-# LANGUAGE RecursiveDo, OverloadedStrings #-}
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
import Control.Monad.Fix

import Debug.Trace

prettyModule mod = ppllvm $ compileModule mod

insertvalue :: MonadIRBuilder m => Operand -> Operand -> [Word32] -> m Operand
insertvalue agg el is = emitInstr (T.typeOf agg) $ AST.InsertValue agg el is []

extractvalue :: MonadIRBuilder m => T.Type -> Operand -> [Word32] -> m Operand
extractvalue ty agg is = emitInstr (ty) $ AST.ExtractValue agg is []

malloc :: MonadIRBuilder m => AST.Operand -> m AST.Operand
malloc size = call (AST.LocalReference mallocTy "malloc") [(size, [])]
  where mallocTy = T.FunctionType (ptr T.void) [T.i32] False

compileModule mod = buildModule "example" $ mdo
  extern "malloc" [T.NamedTypeReference "size_t"] (ptr T.void)

  forM (constructors mod) compileConstructor
  forM (filter isLambda $ bindings mod) compileBinding
  where
  isLambda (NonRec _ Lambda{}) = True
  isLambda _ = False

compileConstructor :: MonadModuleBuilder m => (Name, (Int, Type Name)) -> m ()
compileConstructor (nm, (_, ty)) = do
  typedef (fromString nm) (Just $ T.StructureType False llvmArgTys)

  M.void $ function (fromString $ nm) funArgs (ptr consTy) $ \args -> do
    block `named` "entry" ; do
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
  funArgs = zip llvmArgTy' (repeat (fromString "a"))
  consSize = C.GetElementPtr False (C.Null $ ptr $ consTy) [C.Int 32 1]
  llvmArgTys = T.i64 : llvmArgTy'
  llvmArgTy' = map typeToLlvmType argTys
  argTys = init (unwrapFnType ty)

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

compileBinding :: (MonadFix m, MonadModuleBuilder m) => Bind Var -> m ()
compileBinding (NonRec nm l@(Lambda _ _)) = M.void . function (fromString $ varName nm) args retTy $ \args -> mdo
  block `named` "entry" ; do
    let dict = zipWith (\v op -> (fromString $ varName v, op)) argVars args
    compileBody dict body >>= ret
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

compileBody :: (MonadFix m, MonadIRBuilder m) => [(Id, AST.Operand)] -> CoreExp -> m AST.Operand
compileBody dict (Lit (Double d)) = double d
compileBody dict (Lit (Integer i)) = int64 i
compileBody dict (Var v) = pure . fromJust $ v `lookup` dict
compileBody dict (Case scrut alts) = mdo
  scrutOp <- compileBody dict scrut
  val <- load scrutOp 8
  tag <- extractvalue (T.i64) val [0]

  switch tag defAlt alts'

  (alts', phis) <- unzip <$> M.zipWithM (compileAlt retBlock val) [1..] (filter isConAlt alts)
  defAlt <- (compileDefaultAlt retBlock) (fromJust $ find isTrivialAlt alts)

  retBlock <- block `named` "switch_return" ; do
    phi phis
  where

  compileDefaultAlt retB (TrivialAlt b) = do
    blk <- block `named` "default_branch" ; do
      compileBody dict b
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
    val <- compileBody (bindDict ++ dict) b
    val <- int64 5
    br retB
    let tag = C.Int 64 i
    pure ((tag, block), (val, block))
    where

compileBody _ exp = int64 1
