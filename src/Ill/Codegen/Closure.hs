{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Ill.Codegen.Closure where

import qualified LLVM.AST.Type             as T
import qualified LLVM.AST.Typed            as T
import qualified LLVM.AST.Constant         as C
import qualified LLVM.AST                  as AST
import           LLVM.AST.Operand (Operand(..))
import qualified LLVM.AST.IntegerPredicate as AST
import           LLVM.IRBuilder

import Data.String (fromString)

import Ill.Prelude
import Ill.Codegen.Monad

import Ill.Syntax.Core
import Ill.Syntax.Type

{-
  Closure Representation

  After a lot of head scratching, I've settled on this fairly naive approach to closure representation.

  +------------------+-------------+-----------------+------+--------+-----+
  | Function Pointer | Total Arity | Remaining Arity | ArgN | ArgN-1 | ... |
  +------------------+-------------+-----------------+------+--------+-----+

  Note, the arguments are stored in reverse order. The reason for this is that it allows us to use the
  remaining arity to directly index the closure. If we stored them in forward order, we'd also need to
  store the fully arity in order to determine the offset for the next argument we wish to store.
-}

apply1 :: ModuleM m => m Operand
apply1 =
  function "apply1" [(closureType , "closure"), (ptr T.i8, "argPtr")] (ptr T.i8) $ \[closurePtr, argPtr] -> do
  block `named` "entry"

  totalArity <- flip load 8 =<< (gep closurePtr $ int32 0 ++ int32 1)
  offsetPtr <- gep (ConstantOperand $ C.Null closureType) $ int32 0 ++ int32 3 ++ [totalArity]
  total <- ptrtoint offsetPtr T.i64

  voidPtr <- malloc total
  srcPtr <- bitcast closurePtr (ptr T.i8)
  memcpy voidPtr srcPtr total

  closurePtr <- bitcast voidPtr closureType

  arityPtr <- gep closurePtr $ int32 0 ++ int32 2
  arity <- load arityPtr 8
  cond <- icmp AST.EQ arity (ConstantOperand $ C.Int 8 1)
  condBr cond "Arity 1" "default"

  block `named` "Arity 1"; do
    closureArgPtr <- gep closurePtr $ int32 0 ++ int32 3 ++ int32 0
    store closureArgPtr 8 argPtr
    voidPtrPtr <- gep closurePtr $ int32 0 ++ int32 0
    voidPtr <- load voidPtrPtr 8
    fPtr <- bitcast voidPtr (ptr $ T.FunctionType (ptr $ T.i8) [T.typeOf closurePtr] False)
    retVal <- call fPtr [(closurePtr, [])]
    ret retVal
  block `named` "default"; do
    arity' <- sub arity =<< byte 1
    closureArgPtr <- gep closurePtr $ int32 0 ++ int32 3 ++ [arity']
    store closureArgPtr 8 argPtr

    store arityPtr 8 arity'
    retVal <- bitcast closurePtr (ptr $ T.i8)
    ret retVal

buildClosure :: CodegenM m => BindingInfo -> Var -> m Operand
buildClosure (Info _ argTys retTy _) i@Id{} = do
  voidPtr <- malloc $ sizeofType closureType
  memPtr <- bitcast voidPtr closureType
  val <- load memPtr 8
  let name = AST.ConstantOperand $ C.GlobalReference closureFuncTy (fromString $ "callClosure" ++ varName i)

  voidPtr <- bitcast name (ptr T.i8)
  val' <- insertvalue val voidPtr [0]
  ix <- byte . fromIntegral $ length argTys
  val'' <- insertvalue val'  ix [1]
  val'' <- insertvalue val'' ix [2]

  store memPtr 8 val''
  return memPtr
  where
  closureFuncTy = ptr $ T.FunctionType retTy [closureType] False

mkClosureCall :: ModuleM m => Int -> Var -> m Operand
mkClosureCall arity i@Id{} = function (fromString $ "callClosure" ++ varName i) [(closureType, "closure")] llvmRetTy $ \[closure] -> mdo
  block `named` "entry" ; do
    args <- forM (zip [0..] llvmArgTy') $ \(i, ty) -> do
      argPtr <- gep closure $ int32 0 ++ int32 3 ++ int32 i
      ptr' <- bitcast argPtr (ptr ty)
      load ptr' 8

    let func = AST.ConstantOperand $ C.GlobalReference (ptr $ T.FunctionType llvmRetTy (reverse llvmArgTy') False) (fromString $ varName i)
    retCall <- call func (map (\arg -> (arg, [])) $ reverse args)

    ret retCall
  where
  llvmArgTy' = reverse $ map llvmArgType (init unwrapped)
  llvmRetTy  = llvmArgType (last unwrapped)
  unwrapped = unwrapN arity (idTy i)
