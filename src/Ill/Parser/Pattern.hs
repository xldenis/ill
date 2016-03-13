module Ill.Parser.Pattern where

  import Ill.Syntax
  import Ill.Parser.Lexer


  pattern :: Parser Pattern
  pattern = wildcard <|> destructor <|> var

  destructor :: Parser Pattern
  destructor = do
    cons <- upperIdent
    args <- many pattern
    reutrn Destructor cons args

  nil :: Parser Pattern
  nil = return Nil

  var :: Parser Pattern
  var = PVar <$> identifier

  wildcard :: Parser Pattern
  wildcard = symbol "_" *> (return Wildcard)
