{-# LANGUAGE OverloadedStrings #-}
module Compile where

import Data.Bifunctor (first, bimap)

import Ill.BindingGroup
import Ill.CoreLint
import Ill.Desugar

import Ill.Infer
import Ill.Infer.Monad

import Ill.Syntax as S
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
import LLVM.Transforms
import LLVM.AST (moduleDataLayout)

import qualified LLVM.Relocation as R
import qualified LLVM.CodeModel as CM
import qualified LLVM.CodeGenOpt as CGO
import LLVM.Target

import Paths_ill

import System.IO.Temp
import System.Process

compile :: Maybe String -> Bool -> S.Module SourceSpan -> IO ()
compile outputFile emitLlvm ast = case typeCheckModule ast of
  Left err -> putStrLn $ prettyType err
  Right (mod, env) -> do
    let desugared = defaultPipeline env mod
        core = compileCore desugared
        fileName = moduleName mod <> ".ll"
    withSystemTempFile fileName $ \file handle -> do
      cPath <- getDataFileName "assets/rts.ll"
      withContext $ \ctx -> do
        withModuleFromAST ctx (compileModule core) $ \mod -> do
          withModuleFromLLVMAssembly ctx (File cPath) $ \builtins -> do
            linkModules mod builtins
            let internalizePassSet = defaultPassSetSpec
                  { transforms = [InternalizeFunctions ["main"], SimplifyControlFlowGraph]
                  }
                optPassSpec = defaultCuratedPassSetSpec { optLevel = Just 2}

            withPassManager internalizePassSet $ \pm -> do
              runPassManager pm mod

            withPassManager optPassSpec $ \pm -> do
              runPassManager pm mod

              writeLLVMAssemblyToFile (File file) mod

              when emitLlvm $
                writeLLVMAssemblyToFile (File fileName) mod


      assemble file outputFile
      return ()

assemble objFile outFile = do
  ldFlags <- readProcess "pkg-config" ["--libs", "--static", "bdw-gc"] ""
  callProcess "clang-6.0"
    $ concatMap words (lines ldFlags)
    ++ maybe [objFile] (\file -> [objFile, "-o", file]) outFile

prettyType a = renderIll defaultRenderArgs (pretty $ a)
