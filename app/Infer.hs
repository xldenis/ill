{-# LANGUAGE OverloadedStrings #-}
module Infer where

import Ill.Syntax
import Ill.Infer
import Ill.Infer.Monad
import Ill.Desugar
import Ill.Error
import Ill.Syntax.Pretty

import Control.Monad.State (runStateT)
import Control.Monad.Except (runExcept)

import Prelude hiding (putStrLn, putStr)
import Data.Text.Lazy.IO
import Data.Text.Lazy hiding (map)

infer (Module _ ds) = let
  bg = bindingGroups ds
  typed = runExcept $ runStateT (runCheck $ typeCheck bg) defaultCheckEnv
  in case typed of
    Left e ->
      case e of
        -- UnificationError t1 t2 -> putStrLn "UnificationError: " >> (putStrLn $ prettyType t1) >> (putStrLn $ prettyType t1)
        otherwise -> putStrLn . pack $ show e
    Right (ts, checkState) -> do
      printBG ts

      putStrLn "\nTraits\n"

      void $ forM (traits . env $ checkState) $ putStrLn . prettyTraitInfo

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

prettyTraitInfo (nm, (supers, args, mems)) =
  let topRow = pretty nm <+> (hsep $ map pretty args)
      mems'  = map (\(memNm, ty) -> pretty memNm <+> "::" <+> pretty ty) mems
  in renderIll defaultRenderArgs (nest 2 $ topRow `above` vsep mems')
instance Pretty TypeAnn where
  pretty (Type ty) = pretty ty
  pretty (Kind k) = pretty k
  pretty (None)  = mempty

instance Pretty Kind where
  pretty (Star) = text "*"
  pretty (KFn f a) = pretty f <+> text "->" <+> parensIf (complex a) (pretty a)
    where complex (KFn _ _) = True
          complex a         = False

