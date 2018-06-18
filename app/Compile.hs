{-# LANGUAGE OverloadedStrings #-}
module Compile where

import           Ill.Options

import           Ill.Infer
import           Ill.Infer.Monad

import           Ill.Syntax            as S
import           Ill.Syntax.Core
import           Ill.Syntax.Pretty hiding (nest)

import           Ill.Codegen
import           Ill.Parser
import           Prelude               hiding (putStr, putStrLn)

import           Control.Monad         (when)
import qualified Data.ByteString.Char8 as BS
import           Data.Text.Lazy.IO
import           Data.Bifunctor        (bimap, first)

import qualified LLVM.AST              (moduleDataLayout)
import qualified LLVM.Context          as C
import qualified LLVM.Module           as M
import qualified LLVM.PassManager      as PM
import           LLVM.Transforms
import qualified LLVM.CodeGenOpt       as CGO
import qualified LLVM.CodeModel        as CM
import qualified LLVM.Relocation       as R
import           LLVM.Target

import           Paths_ill

import           System.IO.Temp
import           System.Process

import Options.Applicative.Simple
import Ill.Processor

import Control.Monad.Cont

data Compile = Compile
  { compileFile :: String
  , compileOutputFile :: Maybe String
  , compileEmitLlvm :: Bool
  }

compileParser = Compile <$> fileArg <*> outputFileArg <*> (flag False True $ long "emit-llvm")
  where fileArg = strArgument (metavar "FILE" <> help "location of source file")
        outputFileArg = optional . strOption $ long "output-file" <> metavar "FILTER" <> short 'o'
          <> help "name for the final executable"

compile :: Compile -> GlobalOptions -> Module Name SourceSpan -> IO ()
compile (Compile file outputFile emitLlvm) gOpts prelude = do
  parsedModule <- parseFromFile illParser file
  let coreModules = parsedModule >>= \mod -> do
        let modWithPrelude = mod { moduleImports = Import False All "Prelude" Nothing : (moduleImports mod) }
        modulesToCore (sortModulesByImports [prelude, modWithPrelude])

  case coreModules of
    Left err -> putStrLn . render gOpts $ prettyError err
    Right mods -> do
      let fileName = coreModuleName (head mods) <> ".ll"
          mergedMod = foldl1 mergeCoreModule mods
      withSystemTempFile fileName $ \file handle -> do
        cPath <- getDataFileName "assets/rts.ll"
        C.withContext $ \ctx -> do
          M.withModuleFromLLVMAssembly ctx (M.File cPath) $ \builtins -> do
            nest (map (withModuleFromCore ctx) [mergedMod]) $ \mods -> do
              mod <- foldM (\old new -> M.linkModules new old >> return new) builtins mods

              let internalizePassSet = PM.defaultPassSetSpec
                    { PM.transforms = [InternalizeFunctions ["main"], SimplifyControlFlowGraph]
                    }

              PM.withPassManager internalizePassSet $ \pm -> do
                PM.runPassManager pm mod

                M.writeLLVMAssemblyToFile (M.File file) mod

                when emitLlvm $
                  M.writeLLVMAssemblyToFile (M.File fileName) mod

        assemble file outputFile
        return ()

mergeCoreModule (Mod bind1 cons1 ty1 nm) (Mod bind2 cons2 ty2 _)
  = Mod (bind1 ++ bind2) (cons1 ++ cons2) (ty1 ++ ty2) nm

nest xs = runContT (sequence (map ContT xs))

withModuleFromCore ctx core cont = do
  M.withModuleFromAST ctx (compileModule core) $ \mod -> do
    PM.withPassManager optPassSpec $ \pm -> do
      PM.runPassManager pm mod

    cont mod
  where optPassSpec = PM.defaultCuratedPassSetSpec { PM.optLevel = Just 2}

assemble objFile outFile = do
  ldFlags <- readProcess "pkg-config" ["--libs", "--static", "bdw-gc"] ""
  callProcess "clang-6.0"
    $ concatMap words (lines ldFlags)
    ++ maybe [objFile] (\file -> [objFile, "-o", file]) outFile

render gOpts = renderIll (renderArgs gOpts)
