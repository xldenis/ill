{-# LANGUAGE OverloadedStrings, ConstraintKinds   #-}
module Ill.Codegen.Monad
( module Ill.Codegen.Monad
, functionDefaults
) where

import Ill.Prelude

import           Ill.Syntax.Name
import qualified Ill.Syntax.Core as Core

import           Control.Monad.Fix
import           Control.Monad.Reader

import qualified LLVM.AST.Type             as T
import qualified LLVM.AST.Typed            as T
import qualified LLVM.AST                  as AST
import           LLVM.IRBuilder
import           LLVM.AST.Operand (Operand(..))
import qualified LLVM.AST.Constant         as C
import           LLVM.AST.AddrSpace
import           LLVM.AST.Global
import           LLVM.AST.FunctionAttribute

import           Data.Word

type CodegenM m = (MonadFix m, MonadIRBuilder m,     MonadReader CodegenState m)
type ModuleM  m = (MonadFix m, MonadModuleBuilder m, MonadReader CodegenState m)

insertvalue :: MonadIRBuilder m => Operand -> Operand -> [Word32] -> m Operand
insertvalue agg el is = emitInstr (T.typeOf agg) $ AST.InsertValue agg el is []

extractvalue :: MonadIRBuilder m => T.Type -> Operand -> [Word32] -> m Operand
extractvalue ty agg is = emitInstr ty $ AST.ExtractValue agg is []

byte :: Applicative f => Integer -> f Operand
byte = pure . ConstantOperand . C.Int 8

malloc :: MonadIRBuilder m => AST.Operand -> m AST.Operand
malloc size =
  call (ConstantOperand $ C.GlobalReference mallocTy "malloc") [(size, [])]
  where mallocTy = ptr $ T.FunctionType (ptr T.i8) [T.i64] False

memcpy :: MonadIRBuilder m => AST.Operand -> AST.Operand -> AST.Operand -> m AST.Operand
memcpy dest src size =
  call (ConstantOperand $ C.GlobalReference memcpyTy "memcpy") [(dest, []), (src, []), (size, [])]
  where memcpyTy = ptr $ T.FunctionType (ptr T.i8) [ptr T.i8, ptr T.i8, T.i64] False

functionInlineable = functionDefaults { functionAttributes =  Right InlineHint : functionAttributes functionDefaults }

-- | Define and emit a (non-variadic) function definition
functionWithGlobals
  :: MonadModuleBuilder m
  => Global -- ^ Global IR parameters to set on the function
  -> AST.Name  -- ^ Function name
  -> [(AST.Type, ParameterName)]  -- ^ Parameter types and name suggestions
  -> AST.Type  -- ^ Return type
  -> ([Operand] -> IRBuilderT m ())  -- ^ Function body builder
  -> m Operand
functionWithGlobals defaults label argtys retty body = do
  let tys = fst <$> argtys
  (paramNames, blocks) <- runIRBuilderT emptyIRBuilder $ do
    paramNames <- forM argtys $ \(_, paramName) -> case paramName of
      NoParameterName -> fresh
      ParameterName p -> fresh `named` p
    body $ zipWith LocalReference tys paramNames
    return paramNames
  let
    def = AST.GlobalDefinition defaults
      { name        = label
      , parameters  = (zipWith (\ty nm -> Parameter ty nm []) tys paramNames, False)
      , returnType  = retty
      , basicBlocks = blocks
      }
    funty = ptr $ AST.FunctionType retty (fst <$> argtys) False
  emitDefn def
  pure $ ConstantOperand $ C.GlobalReference funty label

globalVariable nm ty constant = do
  emitDefn $ AST.GlobalDefinition globalVariableDefaults
    { name = nm
    , type' = ty
    , initializer = Just constant
    , isConstant = True
    }
ptr x = T.PointerType x (AddrSpace 0)

data CodegenState = CS
  { globalInfo :: [(Id, BindingInfo)]
  , localInfo  :: [(Id, AST.Operand)]
  , consInfo   :: [(Id, Core.ConstructorEntry)]
  } deriving (Show, Eq)

data BindingInfo = Info
  { arity   :: Int
  , argTys  :: [T.Type]
  , retTy   :: T.Type
  , operand :: AST.Operand
  } deriving (Show, Eq)

bindingType bi = ptr $  T.FunctionType (retTy bi) (argTys bi) False

updateLocals f (CS c l i) = CS c (f l) i

primInt = ptr $ T.NamedTypeReference "Int"
primDouble = ptr $ T.NamedTypeReference "Double"
primBool = ptr $ T.NamedTypeReference "Bool"
primStr  = ptr $ T.NamedTypeReference "String"
