module Ill.Parser.Type where
  import Text.Megaparsec.Text
  import Text.Megaparsec

  import Ill.Syntax
  import Ill.Parser.Lexer

  constructor :: Parser String
  constructor = (:) <$> upperChar <*> (many alphaNumChar)

  typeVar :: Parser Type
  typeVar = TVar <$> (many lowerChar)

  typeName :: Parser Type
  typeName = Name <$> constructor

  typeExp :: Parser Type
  typeExp = typeVar <|> typeName

  typeProduct :: Parser Type
  typeProduct = Constructor <$> constructor <*> (many $ lexeme typeExp)
