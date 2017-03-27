{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Ill.Parser
import Ill.Syntax.Pretty (renderIll, defaultRenderArgs)

import Text.Megaparsec
import Text.PrettyPrint.Free

import Infer

import Options.Generic

import qualified Data.Text.IO as T (readFile)

data Config
  = Build
  | Format String
  | Infer
  deriving (Generic, Show)

instance ParseRecord Config

parseFromFile p file = runParser p file <$> T.readFile file

main :: IO ()
main = do
  config <- getRecord "Ill Compiler" :: IO Config
  case config of
    Build -> putStrLn "build"
    Infer -> infer
    Format file -> do
      res <- parseFromFile illParser file
      case res of
        Right ast -> putStrLn $ renderIll defaultRenderArgs (pretty ast)
        Left err -> putStrLn $ parseErrorPretty err
