module Ill.Parser.Type where
  import Text.Megaparsec.Text
  import Text.Megaparsec

  import Ill.Syntax
  import Ill.Parser.Lexer

  typeVar :: Parser Type
  typeVar = TVar <$> identifier

  typeExp :: Parser Type
  typeExp =  arrow <|> typePrim

  typeProduct :: Parser Type
  typeProduct = Constructor <$> (lexeme capitalized) <*> (many typeExp)

  typePrim :: Parser Type
  typePrim = typeVar <|> typeProduct

  arrow :: Parser Type
  arrow = do
    l <- try $ typePrim <* symbol "->"
    r <- typeExp
    return $ Arrow l r
