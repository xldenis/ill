{-# LANGUAGE RecursiveDo, OverloadedStrings #-}
module Ill.Codegen where

import Ill.Syntax.Core
import Ill.Syntax.Type
import Ill.Syntax.Name

import IRBuilder.Monad
import IRBuilder.Instruction
import qualified LLVM.AST.Type as T
import LLVM.AST.AddrSpace

import LLVM.Pretty

import Data.String
import ModuleBuilder

import Control.Monad (forM)
import qualified Control.Monad as M

import Ill.Prelude hiding (void)

prettyModule mod = ppllvm $ compileModule mod

compileModule mod = buildModule "example" $ mdo
  forM (constructors mod) compileConstructor
  forM (filter isLambda $ bindings mod) compileBinding
  where
  isLambda (NonRec _ Lambda{}) = True
  isLambda _ = False

compileConstructor :: MonadModuleBuilder m => (Name, (Int, Type Name)) -> m ()
compileConstructor (nm, (_, ty)) =
  typedef (fromString nm) (Just $ T.StructureType False llvmArgTys)
  where
  llvmArgTys = map typeToLlvmType argTys
  argTys = init (unwrapFnType ty)

ptr x = T.PointerType x (AddrSpace 0)
typeToLlvmType = ptr . typeToLlvmType'
typeToLlvmType' (TVar nm) = T.NamedTypeReference (fromString nm)
typeToLlvmType' (Arrow a b) = T.FunctionType (typeToLlvmType' b) [typeToLlvmType' a] False
typeToLlvmType' (TAp (TAp (TConstructor "->") a) b) = T.FunctionType (typeToLlvmType' b) [typeToLlvmType' a] False
typeToLlvmType' (TConstructor nm) = T.NamedTypeReference (fromString nm)
typeToLlvmType' t = T.void


compileBinding :: MonadModuleBuilder m => Bind Var -> m ()
compileBinding (NonRec nm l@(Lambda _ _)) = M.void . function (fromString $ varName nm) args ret $ \args -> mdo
  block `named` "entry" ; do
    unreachable
  pure ()

  where
  args = map (\var -> (typeToLlvmType (idTy var), fromString $ varName var)) argVars
  ret  = T.void
  (body, argVars) = unwrapLambda l
  unwrapLambda :: Core Var -> (Core Var, [Var])
  unwrapLambda (Lambda b@(Id{}) e) = (b :) <$> unwrapLambda e
  unwrapLambda (Lambda _ e) = unwrapLambda e
  unwrapLambda e = (e, [])
-- compileBinding exp = unreachable -- error $ show exp

compileBody exp = error $ show exp
