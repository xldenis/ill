module Ill.Parser.Type where
import Control.Applicative

import Text.Megaparsec.Text
import Text.Megaparsec
import Text.Megaparsec.Expr

import Ill.Syntax
import Ill.Parser.Lexer

typeVar :: Parser (Type String)
typeVar = TVar <$> identifier

typeAp :: Parser (Type String)
typeAp = do
  f <- typeCons <|> typeVar
  as <- many $ typeVar <|> parens typeExp

  return $ foldl TAp f as

typeCons :: Parser (Type String)
typeCons = TConstructor <$> lexeme capitalized

typeProduct :: Parser (Type String)
typeProduct = do
  f <- typeCons
  as <- many (typeVar <|> parens typeExp)

  return $ foldl TAp f as

trait :: Parser (Constraint String)
trait =  (,) <$> upperIdent <*> some typeExp

constraints :: Parser [Constraint Name]
constraints = try $ trait `sepBy1` symbol "," <* symbol "|"

constrainedType :: Parser (Type String)
constrainedType = Constrained <$> constraints <*> typeExp

fullType :: Parser (Type String)
fullType = constrainedType <|> typeExp

typeExp = makeExprParser (typePrim <|> parens typeExp) opTable
  where
  opTable = [[InfixR $ lexeme $ string "->" *> return Arrow]]

  typePrim :: Parser (Type String)
  typePrim =  typeAp <|> typeVar
