{-# LANGUAGE RecursiveDo, OverloadedStrings #-}
module Ill.Codegen where

import Ill.Syntax.Core
import Ill.Syntax.Type

import IRBuilder.Monad
import IRBuilder.Instruction
import LLVM.AST.Type as AST
import LLVM.AST.Name
import LLVM.Pretty

import Data.String
import ModuleBuilder

import Control.Monad (forM)

import Ill.Prelude hiding (void)

prettyModule mod = ppllvm $ compileModule mod

compileModule mod = buildModule "example" $ mdo
  forM (bindings mod) compileBinding

compileBinding (NonRec nm l@(Lambda _ _)) = function (Name . fromString $ varName nm) args ret $ \args -> mdo
  block `named` "entry" ; do
    unreachable

  pure ()
  where
  args = []
  ret  = void
  unwrapLambda :: Core Var -> (Core Var, [Var])
  unwrapLambda (Lambda b@(Id{}) e) = (b :) <$> unwrapLambda e
  unwrapLambda (Lambda _ e) = unwrapLambda e
  unwrapLambda e = (e, [])

compileBody exp = error $ show exp
