module CodegenDebug where

import Data.Bifunctor (first, bimap)

import Ill.Options
import Ill.BindingGroup
import Ill.CoreLint
import Ill.Desugar

import Ill.Infer
import Ill.Infer.Monad

import Ill.Syntax
import Ill.Syntax.Core
import Ill.Syntax.Pretty

import Ill.Codegen
import Ill.Renamer

import Prelude hiding (putStrLn, putStr)

import Data.Text.Lazy.IO
import qualified Data.ByteString.Char8 as BS
import Control.Monad (when)

import LLVM.Module
import LLVM.Context
import LLVM.PassManager

import Paths_ill

codegen toPrint gOpts ast = case (execTypecheckModule) ast of
  Left err -> putStrLn $ render gOpts (prettyError err)
  Right (mod, env) -> do
    let desugared = defaultPipeline env mod
        core = compileCore desugared
        binds = (bindings core)

    when toPrint $ putStrLn (prettyModule core)

    withContext $ \ctx -> do
      cPath <- getDataFileName "assets/rts.ll"
      withModuleFromAST ctx (compileModule core) $ \mod -> do
        withModuleFromLLVMAssembly ctx (File cPath) $ \builtins -> do
          withPassManager defaultCuratedPassSetSpec $ \pm -> do
            runPassManager pm mod
            linkModules mod builtins
            writeLLVMAssemblyToFile (File "example.ll") mod



      return ()

render opts = renderIll (renderArgs opts)
