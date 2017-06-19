module Infer where

import Ill.Syntax
import Ill.Infer
import Ill.Infer.Monad
import Ill.Desugar

import Ill.Syntax.Pretty

import Control.Monad.State (runStateT)
import Control.Monad.Except (runExcept)

infer (Module _ ds) = let
  bg = bindingGroups ds
  typed = runExcept $ runStateT (runCheck $ typeCheck bg) defaultCheckEnv
  in case typed of
    Left e -> putStrLn e
    Right (ts, _) -> printBG ts

printBG ((ValueBG ds):bgs) = printTypes ds >> printBG bgs
printBG ((DataBG  ds):bgs) = printTypes ds >> printBG bgs
printBG (_ : bgs) = printBG bgs
printBG []        = return ()

printTypes :: [Decl TypedAnn] -> IO ()
printTypes ((a :< Value n _):ts)   = putStr (n ++ ": ") >> putStrLn (prettyType a) >> printTypes ts
printTypes ((a :< Data  n _ _):ts) = putStr (n ++ ": ") >> putStrLn (prettyType a) >> printTypes ts
printTypes (_ : ts) = printTypes ts
printTypes [] = return ()

prettyType a = renderIll defaultRenderArgs (pretty $ ty a)

instance Pretty TypeAnn where
  pretty (Type ty) = pretty ty
  pretty (Kind k) = pretty k
  pretty (None)  = mempty

instance Pretty Kind where
  pretty (Star) = text "*"
  pretty (KFn f a) = pretty f <+> text "->" <+> parensIf (complex a) (pretty a)
    where complex (KFn _ _) = True
          complex a         = False
