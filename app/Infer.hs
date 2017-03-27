module Infer where

import Data.Text.IO as T (getLine)

import Ill.Inference
import Ill.Parser.Expression

import Ill.Syntax
import Ill.Syntax.Pretty (renderIll, defaultRenderArgs, pretty)

import Text.Megaparsec

parseFromIO p = runParser p "io" <$> T.getLine

infer :: IO ()
infer = do
  infer'
  infer

infer' :: IO ()
infer' = do
  exp <- parseFromIO (fullExpr <* eof)

  let x = exp >>= \exp -> do
          -- return $ bleh initialEnv [] exp
          return $ snd <$> tiExpr initialEnv [] exp
  case x of
    Left e -> putStrLn (parseErrorPretty e)
    Right ty -> do
      let r = getType ty
      print (rTI ty)
      print r
      putStrLn $ renderIll defaultRenderArgs (pretty (ty2sTy r))

getType :: Types a => TI a -> a
getType (TI f) = let (s, _, x) = f nullSubst 0 in apply s x

rTI (TI f) = f nullSubst 0
