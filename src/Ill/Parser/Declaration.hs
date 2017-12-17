{-# LANGUAGE OverloadedStrings #-}
module Ill.Parser.Declaration (declaration) where

import           Ill.Prelude

import Control.Monad (when)

import Data.List (intercalate)
import Data.Maybe

import Text.Megaparsec

import Ill.Syntax
import Ill.Parser.Lexer
import Ill.Parser.Pattern
import Ill.Parser.Type
import Ill.Parser.Expression


declaration :: Parser (Decl SourceSpan)
declaration = choice
  [ dataDeclaration, typeSynonymDeclaration, importDeclaration,
    valueDeclaration, signatureDeclaration, traitDeclaration, implDeclaration
  ]

dataDeclaration :: Parser (Decl SourceSpan)
dataDeclaration = label "data type" . withLoc $ do
  symbol "data"
  name <- upperIdent
  vars <- many identifier
  symbol "="
  types <- typeProduct `sepBy1` (lexeme $ char '|')
  return $ Data name vars types

typeSynonymDeclaration :: Parser (Decl SourceSpan)
typeSynonymDeclaration = label "type synonym" .  withLoc $ do
  try $ symbol "type"
  alias <- upperIdent
  vars  <- many identifier
  symbol "="
  aliasee <- typeProduct
  return $ TypeSynonym alias vars aliasee

traitDeclaration :: Parser (Decl SourceSpan)
traitDeclaration = label "trait declaration" . withLoc $ do
  symbol "trait"
  supers <- constraintP <|> (pure [])
  name <- lexeme capitalized
  args <- some identifier
  sep
  body <- manyTill (signatureDeclaration <* (sep <* scn)) $ symbol "end"
  return $ TraitDecl supers name args body

implDeclaration :: Parser (Decl SourceSpan)
implDeclaration = label "trait implementation" . withLoc $ do
  symbol "impl"
  trt <- fullType
  let (constraints, ty) = unconstrained trt
      className : vars  = unwrapProduct ty

  className' <- case className of
    TConstructor t -> return t
    a -> fail $ "`" ++ show a ++ "` is not a valid class name."
  sep
  body <- manyTill (valueDeclaration <* sep <* scn) $ symbol "end"
  return $ TraitImpl constraints className' vars body

signatureDeclaration :: Parser (Decl SourceSpan)
signatureDeclaration = label "value signature" . withLoc $ do
  ident <- try $ identifier <* symbol "::"
  Signature ident <$> fullType

valueDeclaration :: Parser (Decl SourceSpan)
valueDeclaration = label "value" . withLoc $ do
  symbol "fn"
  name <- identifier
  main <- branch
  alts <- many $ do
    symbol "or"
    bname <- symbol $ pack name
    branch
  symbol "end"
  return $ Value name (main : alts)
  where
  branch = do
    args <- parens $ list pattern
    scn
    body <- body
    scn
    return (args, body)

importDeclaration :: Parser (Decl SourceSpan)
importDeclaration = label "import" . withLoc $ do
  symbol "import"
  qual <- qualified
  path <- intercalate "." <$> (lexeme $ capitalized `sepBy` (char '.'))
  alias <- if qual then Just <$> alias else optional alias
  imports <- mask

  return $ Import qual imports path alias
  where qualified = isJust <$> (optional $ symbol "qualified")
        alias = symbol "as" *> upperIdent
        mask = (try $ do
          cons <- (symbol "hiding" *> return Hiding) <|> (return Only)
          args <- parens $ list identifier
          return $ cons args) <|> (return All)
