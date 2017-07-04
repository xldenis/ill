module Ill.Parser.Type where
import Control.Applicative

import Text.Megaparsec.Text
import Text.Megaparsec

import Ill.Syntax
import Ill.Parser.Lexer

typeVar :: Parser (Type String)
typeVar = TVar <$> identifier

typeExp :: Parser (Type String)
typeExp =  arrow <|> typePrim <|> parens typeExp

typeAp :: Parser (Type String)
typeAp = do
  f <- typeCons <|> typeVar
  as <- many typeExp

  return $ foldl (TAp) f as

typeCons :: Parser (Type String)
typeCons = TConstructor <$> lexeme capitalized

typeProduct :: Parser (Type String)
typeProduct = do
  f <- typeCons
  as <- many (typeVar <|> parens typeExp)

  return $ foldl TAp f as

typePrim :: Parser (Type String)
typePrim =  typeAp <|> typeVar

arrow :: Parser (Type String)
arrow =  do
  l <- try $ (typePrim <|> parens typeExp) <* symbol "->"
  r <- typeExp
  return $ Arrow l r

trait :: Parser (Constraint String)
trait =  (,) <$> upperIdent <*> some typeExp

constraints :: Parser [Constraint Name]
constraints = try $ trait `sepBy` symbol "," <* symbol "|"

constrainedType :: Parser (Type String)
constrainedType =  Constrained <$> (constraints <|> return []) <*> typeExp
