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

typeProduct :: Parser (Type String)
typeProduct =  Constructor <$> lexeme capitalized <*> many typeExp

typePrim :: Parser (Type String)
typePrim =  typeProduct <|> typeVar

arrow :: Parser (Type String)
arrow =  do
  l <- try $ typePrim <* symbol "->"
  r <- typeExp
  return $ Arrow l r

trait :: Parser (Type String)
trait =  Trait <$> upperIdent <*> typeExp

constraints :: Parser [Type String]
constraints = try $ trait `sepBy` symbol "," <* symbol "|"

constrainedType :: Parser (Type String)
constrainedType =  Constraint <$> (constraints <|> return []) <*> typeExp
