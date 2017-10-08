{-# LANGUAGE OverloadedStrings #-}
module Infer where

import Ill.Syntax
import Ill.Infer
import Ill.Infer.Monad
import Ill.BindingGroup
import Ill.Error
import Ill.Syntax.Pretty

import Control.Monad.State (runStateT)
import Control.Monad.Except (runExcept)

import Prelude hiding (putStrLn, putStr)
import Data.Text.Lazy.IO
import Data.Text.Lazy hiding (map)

infer m = let
  typed = runTC m
  in case typed of
    Left e ->
      case e of
        -- UnificationError t1 t2 -> putStrLn "UnificationError: " >> (putStrLn $ prettyType t1) >> (putStrLn $ prettyType t1)
        otherwise -> putStrLn $ prettyType e
    Right (ts, checkState) -> do
      printBG ts

      putStrLn "\nTraits\n"

      void $ forM (traits . env $ checkState) $ putStrLn . prettyTraitInfo

runTC (Module _ ds) = unCheck (bindingGroups ds >>= typeCheck)

unCheck c = runExcept $ runStateT (runCheck c) defaultCheckEnv

printBG ((ValueBG ds):bgs) = printTypes ds >> printBG bgs
printBG ((DataBG  ds):bgs) = printTypes ds >> printBG bgs
printBG (_ : bgs) = printBG bgs
printBG []        = return ()

printTypes :: [Decl TypedAnn] -> IO ()
printTypes ((a :< Value n _):ts)   = putStr (pack n <> ": ") >> putStrLn (prettyType $ ty a) >> printTypes ts
printTypes ((a :< Data  n _ _):ts) = putStr (pack n <> ": ") >> putStrLn (prettyType $ ty a) >> printTypes ts
printTypes (_ : ts) = printTypes ts
printTypes [] = return ()

prettyType a = renderIll defaultRenderArgs (pretty $ a)

prettyTraitInfo (nm, TraitEntry supers args mems) =
  let topRow = pretty nm <+> (hsep $ map pretty args)
      mems'  = map (\(memNm, ty) -> pretty memNm <+> "::" <+> pretty ty) mems
  in renderIll defaultRenderArgs (nest 2 $ topRow `above` vsep mems')

instance Pretty TypeAnn where
  pretty (Type ty) = pretty ty
  pretty (Kind k) = pretty k
  pretty (None)  = mempty

