module CodegenDebug where

import Data.Bifunctor (first, bimap)

import Thrill.Options
import Thrill.BindingGroup
import Thrill.CoreLint
import Thrill.Desugar

import Thrill.Infer
import Thrill.Infer.Monad

import Thrill.Syntax
import Thrill.Syntax.Core
import Thrill.Syntax.Pretty

import Thrill.Codegen
import Thrill.Renamer

import Prelude hiding (putStrLn, putStr)

import Data.Text.Lazy.IO
import qualified Data.ByteString.Char8 as BS
import Control.Monad (when)

import LLVM.Module
import LLVM.Context
import LLVM.PassManager

import Paths_thrill

codegen toPrint gOpts ast = case (typeCheckModule) ast of
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

render opts = renderThrill (renderArgs opts)
