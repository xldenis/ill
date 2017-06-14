module Ill.Parser.Declaration (declaration) where

import Control.Monad (when)

import Data.List (intercalate)
import Data.Maybe

import Text.Megaparsec.Text
import Text.Megaparsec

import Ill.Syntax
import Ill.Parser.Lexer
import Ill.Parser.Pattern
import Ill.Parser.Type
import Ill.Parser.Expression


declaration :: Parser (Decl SourceSpan)
declaration =
  dataDeclaration <|> typeSynonymDeclaration <|> importDeclaration <|>
  valueDeclaration <|> signatureDeclaration <|> traitDeclaration <|> implDeclaration

-- Need to add type variables!!!

dataDeclaration :: Parser (Decl SourceSpan)
dataDeclaration = withLoc $ do
  symbol "data"
  name <- upperIdent
  vars <- many identifier
  symbol "="
  types <- typeProduct `sepBy` (lexeme $ char '|')
  return $ Data name types

typeSynonymDeclaration :: Parser (Decl SourceSpan)
typeSynonymDeclaration = withLoc $ do
  try $ symbol "type"
  alias <- upperIdent
  vars  <- many identifier
  symbol "="
  aliasee <- typeProduct
  return $ TypeSynonym alias vars aliasee

traitDeclaration :: Parser (Decl SourceSpan)
traitDeclaration = withLoc $ do
  symbol "trait"
  trt <- constrainedType
  sep
  body <- manyTill (valueDeclaration <|> signatureDeclaration <* (sep <* scn)) $ symbol "end"
  return $ TraitDecl trt body

implDeclaration :: Parser (Decl SourceSpan)
implDeclaration = withLoc $ do
  symbol "impl"
  trt <- constrainedType
  sep
  body <- manyTill (valueDeclaration <* sep <* scn) $ symbol "end"
  return $ TraitImpl trt body

signatureDeclaration :: Parser (Decl SourceSpan)
signatureDeclaration = try $ withLoc $ do
  ident <- identifier
  symbol "::"
  Signature ident <$> constrainedType

-- TODO: Argument pattern matching?
valueDeclaration :: Parser (Decl SourceSpan)
valueDeclaration = withLoc $ do
  symbol "fn"
  name <- identifier
  main <- branch
  alts <- many $ do
    symbol "or"
    bname <- identifier
    when (bname /= name) $ fail ("Invalid function alternative for " ++ name)
    branch
  symbol "end"
  return $ Value name (main : alts)
  where branch = do
                  args <- parens $ list pattern
                  scn
                  body <- body
                  scn
                  return (args, body)

importDeclaration :: Parser (Decl SourceSpan)
importDeclaration = withLoc $ do
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
