{-# LANGUAGE OverloadedStrings #-}
module Thrill.Parser.Type where

import           Thrill.Prelude

import Text.Megaparsec (sepBy1, try, label)
import Text.Megaparsec.Expr

import Thrill.Syntax
import Thrill.Parser.Lexer

typeVar :: Parser (Type String)
typeVar = label "type variable" $ TVar <$> identifier

typeAp :: Parser (Type String)
typeAp = do
  f <- typeCons <|> typeVar
  as <- many $ typeCons <|> typeVar <|> parens typeExp

  return $ foldl TAp f as

typeCons :: Parser (Type String)
typeCons = TConstructor <$> lexeme capitalized

typeProduct :: Parser (Type String)
typeProduct = label "product type" $ do
  f <- typeCons
  as <- many (typeCons <|> typeVar <|> parens typeExp)

  return $ foldl TAp f as

trait :: Parser (Constraint String)
trait = label "trait" $ (,) <$> upperIdent <*> typeExp

constraintP :: Parser [Constraint Name]
constraintP = label "type constraint" $ try $ trait `sepBy1` symbol "," <* symbol "|"

constrainedType :: Parser (Type String)
constrainedType = label "constrained type" $ Constrained <$> constraintP <*> typeExp

fullType :: Parser (Type String)
fullType = label "type" $ constrainedType <|> typeExp

typeExp = makeExprParser (typePrim <|> parens typeExp) opTable
  where
  opTable = [[InfixR $ lexeme $ string "->" *> return Arrow]]

  typePrim :: Parser (Type String)
  typePrim =  typeAp <|> typeVar
