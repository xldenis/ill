module CodegenDebug where

import Data.Bifunctor (first, bimap)

import Ill.BindingGroup
import Ill.CoreLint
import Ill.Desugar

import Ill.Infer
import Ill.Infer.Monad

import Ill.Syntax
import Ill.Syntax.Core
import Ill.Syntax.Pretty

import Ill.Codegen

import Prelude hiding (putStrLn, putStr)

import Data.Text.Lazy.IO
import qualified Data.ByteString.Char8 as BS

import LLVM.Module
import LLVM.Context

codegen ast = case runTC ast of
  Left err -> putStrLn $ prettyType err
  Right (typed, env) -> do
    let desugared = defaultPipeline env typed
        core = compileCore desugared
        binds = (bindings core)
    putStrLn $ prettyModule core

    withContext $ \ctx -> do
      llvm <- withModuleFromAST ctx (compileModule core) moduleLLVMAssembly
      BS.putStrLn llvm


runTC (Module _ ds) = execCheck (bindingGroups ds >>= typeCheck) >>= pure . bimap fromBindingGroups env
prettyType a = renderIll defaultRenderArgs (pretty $ a)
