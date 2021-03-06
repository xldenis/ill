{-# LANGUAGE OverloadedStrings #-}
module Compile where

import           Thrill.BindingGroup
import           Thrill.CoreLint
import           Thrill.Desugar
import           Thrill.Options
import           Thrill.Renamer

import           Thrill.Infer
import           Thrill.Infer.Monad

import           Thrill.Syntax            as S
import           Thrill.Syntax.Core
import           Thrill.Syntax.Pretty

import           Thrill.Codegen

import           Prelude               hiding (putStr, putStrLn)

import           Control.Monad         (when)
import qualified Data.ByteString.Char8 as BS
import           Data.Text.Lazy.IO
import           Data.Bifunctor        (bimap, first)

import           LLVM.AST              (moduleDataLayout)
import           LLVM.Context
import           LLVM.Module
import           LLVM.PassManager
import           LLVM.Transforms

import qualified LLVM.CodeGenOpt       as CGO
import qualified LLVM.CodeModel        as CM
import qualified LLVM.Relocation       as R
import           LLVM.Target

import           Paths_thrill

import           System.IO.Temp
import           System.Process

compile :: Maybe String -> Bool -> GlobalOptions -> RenamedModule SourceSpan -> IO ()
compile outputFile emitLlvm gOpts ast = case (typeCheckModule) ast of
  Left err -> putStrLn . render gOpts $ prettyError err
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

render gOpts = renderThrill (renderArgs gOpts)
