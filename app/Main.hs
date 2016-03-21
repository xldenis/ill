{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Ill.Parser
import Ill.Syntax.Pretty (renderIll, defaultRenderArgs)

import Text.Megaparsec
import Text.PrettyPrint.Free

import Options.Generic

data Config
  = Build
  | Format String
  deriving (Generic, Show)

instance ParseRecord Config

main :: IO ()
main = do
  config <- getRecord "Ill Compiler" :: IO Config
  case config of
    Build -> putStrLn "build"
    Format file -> do
      putStrLn "format"
      res <- parseFromFile illParser file
      case res of
        Right ast -> putStrLn $ renderIll defaultRenderArgs (pretty ast)
        Left err -> putStrLn $ show err
