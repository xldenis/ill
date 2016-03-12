module Ill.Parser.Type where
  import Text.Megaparsec.Text
  import Text.Megaparsec

  import Ill.Syntax
  import Ill.Parser.Lexer

  typeVar :: Parser Type
  typeVar = TVar <$> identifier

  typeName :: Parser Type
  typeName = Name <$> lexeme capitalized

  typeExp :: Parser Type
  typeExp = typeVar -- <|> typeName

  typeProduct :: Parser Type
  typeProduct = Constructor <$> (lexeme capitalized) <*> (many typeExp)
