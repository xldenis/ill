{-# LANGUAGE RecursiveDo, OverloadedStrings #-}
module Ill.Codegen where

import Control.Monad (forM)
import Data.String

import Ill.Syntax.Core
import Ill.Syntax.Literal
import Ill.Syntax.Name
import Ill.Syntax.Type

import LLVM.AST.AddrSpace
import LLVM.IRBuilder
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.Pretty
import qualified Control.Monad as M
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Type as T
import qualified LLVM.AST.Typed as T
import LLVM.AST.Operand

import Ill.Prelude hiding (void)
import Data.Word

prettyModule mod = ppllvm $ compileModule mod

insertvalue :: MonadIRBuilder m => Operand -> Operand -> [Word32] -> m Operand
insertvalue agg el is = emitInstr (T.typeOf agg) $ AST.InsertValue agg el is []

extractvalue :: MonadIRBuilder m => Operand -> [Word32] -> m Operand
extractvalue agg is = emitInstr (T.typeOf agg) $ AST.ExtractValue agg is []

malloc :: MonadIRBuilder m => AST.Operand -> m AST.Operand
malloc size = call (AST.LocalReference mallocTy "malloc") [(size, [])]
  where mallocTy = T.FunctionType (ptr T.void) [T.i32] False

compileModule mod = buildModule "example" $ mdo
  extern "malloc" [T.NamedTypeReference "size_t"] (ptr T.void)

  forM (constructors mod) compileConstructor
  -- forM (filter isLambda $ bindings mod) compileBinding
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
typeToLlvmType' (Arrow a b) = T.FunctionType (typeToLlvmType' b) [typeToLlvmType' a] False
typeToLlvmType' (TAp (TAp (TConstructor "->") a) b) = T.FunctionType (typeToLlvmType' b) [typeToLlvmType' a] False
typeToLlvmType' (TConstructor nm) = T.NamedTypeReference (fromString nm)
typeToLlvmType' t = T.void

compileBinding :: MonadModuleBuilder m => Bind Var -> m ()
compileBinding (NonRec nm l@(Lambda _ _)) = M.void . function (fromString $ varName nm) args retTy $ \args -> mdo
  block `named` "entry" ; do
    ret =<< compileBody body
    -- unreachable
  pure ()

  where
  args = map (\var -> (typeToLlvmType (idTy var), fromString $ varName var)) argVars
  retTy = T.void
  (body, argVars) = unwrapLambda l
  unwrapLambda :: Core Var -> (Core Var, [Var])
  unwrapLambda (Lambda b@(Id{}) e) = (b :) <$> unwrapLambda e
  unwrapLambda (Lambda _ e) = unwrapLambda e
  unwrapLambda e = (e, [])
-- compileBinding exp = unreachable -- error $ show exp

compileBody :: MonadIRBuilder m => CoreExp -> m AST.Operand
compileBody (Lit (Double d)) = double d
compileBody (Lit (Integer i)) = int64 i
-- compileBody (Case scrut alts) = do
--   scrutOp <- compileBody scrut
--   val <- load scrutOp 8
--   tag <- extractvalue val [0]

compileBody exp = int64 1
