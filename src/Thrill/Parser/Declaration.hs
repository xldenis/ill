{-# LANGUAGE OverloadedStrings #-}
module Thrill.Parser.Declaration (declaration) where

import           Thrill.Prelude

import Text.Megaparsec hiding (many, some)

import Thrill.Syntax
import Thrill.Parser.Lexer
import Thrill.Parser.Pattern
import Thrill.Parser.Type
import Thrill.Parser.Expression


declaration :: Parser (Decl Name SourceSpan)
declaration = label "top-level declaration" $ choice
  [ dataDeclaration, typeSynonymDeclaration, importDeclaration,
    valueDeclaration, signatureDeclaration, traitDeclaration, implDeclaration
  ]

dataDeclaration :: Parser (Decl Name SourceSpan)
dataDeclaration = label "data type" . withLoc $ do
  symbol "data"
  name <- label "type name" upperIdent
  vars <- many (label "type variable" identifier)
  symbol "="
  types <- typeProduct `sepBy1` (label "sum type" . lexeme $ char '|')
  return $ Data name vars types

typeSynonymDeclaration :: Parser (Decl Name SourceSpan)
typeSynonymDeclaration = label "type synonym" .  withLoc $ do
  try $ symbol "type"
  alias <- upperIdent
  vars  <- many identifier
  symbol "="
  aliasee <- typeProduct
  return $ TypeSynonym alias vars aliasee

traitDeclaration :: Parser (Decl Name SourceSpan)
traitDeclaration = label "trait declaration" . withLoc $ do
  symbol "trait"
  supers <- constraintP <|> (pure [])
  name <- lexeme capitalized
  arg <- identifier
  sep
  body <- manyTill (signatureDeclaration <* (sep <* scn)) $ symbol "end"
  return $ TraitDecl supers name arg body

implDeclaration :: Parser (Decl Name SourceSpan)
implDeclaration = label "trait implementation" . withLoc $ do
  symbol "impl"
  trt <- fullType
  let (constraints, ty) = unconstrained trt
      className : vars  = unwrapProduct ty

  implVar <- case vars of
    [] -> fail "not enough vars"
    [ty] -> pure ty
    _ -> fail "too many vars"

  className' <- case className of
    TConstructor t -> return t
    a -> fail $ "`" ++ show a ++ "` is not a valid class name."
  sep
  body <- manyTill (valueDeclaration <* sep <* scn) $ symbol "end"
  return $ TraitImpl constraints className' implVar body

signatureDeclaration :: Parser (Decl Name SourceSpan)
signatureDeclaration = label "value signature" . withLoc $ do
  ident <- try $ identifier <* symbol "::"
  Signature ident <$> fullType

valueDeclaration :: Parser (Decl Name SourceSpan)
valueDeclaration = label "value" . withLoc $ do
  symbol "fn"
  name <- label "value name" identifier
  main <- branch
  alts <- many $ do
    symbol "or"
    bname <- symbol $ pack name
    branch
  symbol "end"
  return $ Value name (main : alts)
  where
  branch = do
    args <- label "argument list" $ parens $ list pattern
    scn
    body <- body
    scn
    return (args, body)

importDeclaration :: Parser (Decl Name SourceSpan)
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
