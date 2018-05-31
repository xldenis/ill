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
import Control.Monad (when)

import LLVM.Module
import LLVM.Context
import LLVM.PassManager

import Paths_ill

codegen toPrint ast = case runTC ast of
  Left err -> putStrLn $ prettyType err
  Right (typed, env) -> do
    let desugared = defaultPipeline env typed
        core = compileCore desugared
        binds = (bindings core)

    when toPrint $ putStrLn (prettyModule core)

    withContext $ \ctx -> do
      path <- getDataFileName "assets/builtins.ll"
      cPath <- getDataFileName "assets/rts.ll"
      withModuleFromAST ctx (compileModule core) $ \mod -> do
        withModuleFromLLVMAssembly ctx (File path) $ \builtins -> do
          withModuleFromLLVMAssembly ctx (File cPath) $ \cBuiltins -> do
            withPassManager defaultCuratedPassSetSpec $ \pm -> do
              runPassManager pm mod
              linkModules mod builtins
              linkModules mod cBuiltins
              writeLLVMAssemblyToFile (File "example.ll") mod



      return ()

runTC (Module _ ds) = execCheck (bindingGroups ds >>= typeCheck) >>= pure . bimap fromBindingGroups env
prettyType a = renderIll defaultRenderArgs (pretty $ a)
