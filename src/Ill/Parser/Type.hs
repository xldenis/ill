module Ill.Parser.Type where
  import Control.Applicative

  import Text.Megaparsec.Text
  import Text.Megaparsec

  import Ill.Syntax
  import Ill.Parser.Lexer

  typeVar :: Parser Type
  typeVar = (Fix . TVar) <$> identifier

  typeExp :: Parser Type
  typeExp =  arrow <|> typePrim

  typeProduct :: Parser Type
  typeProduct = Fix <$> (Constructor <$> lexeme capitalized <*> many typeExp)

  typePrim :: Parser Type
  typePrim =  typeProduct <|> typeVar

  arrow :: Parser Type
  arrow = Fix <$> do
    l <- try $ typePrim <* symbol "->"
    r <- typeExp
    return $ Arrow l r

  trait :: Parser Type
  trait = Fix <$> (Trait <$> upperIdent <*> typeExp)

  constraints :: Parser [Type]
  constraints = try $ trait `sepBy` symbol "," <* symbol "|"

  constrainedType :: Parser Type
  constrainedType = Fix <$> (Constraint <$> (constraints <|> return []) <*> typeExp)
