{-# LANGUAGE OverloadedStrings, ConstraintKinds   #-}
module Thrill.Codegen.Monad
( module Thrill.Codegen.Monad
, functionDefaults
) where

import Thrill.Prelude

import           Thrill.Syntax.Name
import qualified Thrill.Syntax.Core as Core
import           Thrill.Syntax.Core hiding (name)
import qualified Thrill.Syntax.Builtins as Builtins
import           Thrill.Syntax.Type

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
import           Data.String

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
  call (ConstantOperand $ C.GlobalReference mallocTy "GC_malloc") [(size, [])]
  where mallocTy = ptr $ T.FunctionType (ptr T.i8) [T.i64] False


-- This should only be used if the memory will not contain any pointers
-- the gc will not check this area during garbage collection
mallocAtomic :: MonadIRBuilder m => AST.Operand -> m AST.Operand
mallocAtomic size =
  call (ConstantOperand $ C.GlobalReference mallocTy "GC_malloc_atomic") [(size, [])]
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

sizeofType ty = ConstantOperand $ C.PtrToInt (C.GetElementPtr False (C.Null ty) [C.Int 32 1]) T.i64

ptr x = T.PointerType x (AddrSpace 0)

primInt = ptr $ T.NamedTypeReference "Int"
primDouble = ptr $ T.NamedTypeReference "Double"
primBool = ptr $ T.NamedTypeReference "Bool"
primStr  = ptr $ T.NamedTypeReference "String"

closureType = ptr $ T.StructureType False (ptr T.i8  : T.i8 : T.i8 : [T.ArrayType 1 $ ptr T.i8])

llvmArgType :: Type QualifiedName -> AST.Type
llvmArgType (TVar nm) = ptr T.i8
llvmArgType (TConstructor nm) = ptr $ T.NamedTypeReference (fromString $ qualName nm)
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
llvmArgType f@(TAp (TAp ArrowConstructor a) b) =
  closureType
  where
  llvmArgTy' = reverse $ map llvmArgType argTys
  argTys = init unwrapped
  unwrapped = unwrapFnType f
llvmArgType t = error $ show t

data CodegenState = CS
  { globalInfo :: [(QualifiedName, BindingInfo)]
  , localInfo  :: [(QualifiedName, AST.Operand)]
  , consInfo   :: [(QualifiedName, Core.ConstructorEntry)]
  } deriving (Show, Eq)

fromModule mod = CS (infoMap mod) [] (constructors mod)

infoMap mod =
  map collectBindingInfo (bindings mod) ++
  map collectConstructorInfo (constructors mod) ++
  builtinInfo

  where

  builtinInfo :: [(QualifiedName, BindingInfo)]
  builtinInfo = map go Builtins.primitives
    where
    go (nm, ty) = let
      arity = length args
      tys = unwrapFnType ty
      args = map llvmArgType $ init tys
      ret = llvmArgType $ last tys
      op  = ConstantOperand $ C.GlobalReference (ptr $ T.FunctionType ret args False) (fromString $ qualName nm)
      in (nm, Info arity args ret op)

  collectConstructorInfo :: (QualifiedName, Core.ConstructorEntry) -> (QualifiedName, BindingInfo)
  collectConstructorInfo (nm, cons) = let
    tys = unwrapFnType (consType cons)
    args = map llvmArgType $ init tys
    ret  = llvmArgType $ last tys

    op = ConstantOperand $ C.GlobalReference (ptr $ T.FunctionType ret args False) (fromString $ qualName nm)

    in (nm, Info (consArity cons) args ret op)

  collectBindingInfo :: Core.Bind Core.Var -> (QualifiedName, BindingInfo)
  collectBindingInfo (Core.NonRec v b) = let
    arity = length . snd $ unwrapLambda b
    tys   = unwrapN arity (idTy v)
    args  = map llvmArgType $ init tys
    ret   = llvmArgType $ last tys
    op    = ConstantOperand $ C.GlobalReference (ptr $ T.FunctionType ret args False) (fromString $ qualName $ varName v)
    in (varName v, Info arity args ret op)

data BindingInfo = Info
  { arity   :: Int
  , argTys  :: [T.Type]
  , retTy   :: T.Type
  , operand :: AST.Operand
  } deriving (Show, Eq)

bindingType bi = ptr $  T.FunctionType (retTy bi) (argTys bi) False

updateLocals f (CS c l i) = CS c (f l) i

