module Ill.Parser.Declaration where

  import Data.List (intercalate)
  import Data.Maybe

  import Text.Megaparsec.Text
  import Text.Megaparsec

  import Ill.Syntax
  import Ill.Parser.Lexer
  import Ill.Parser.Type
  import Ill.Parser.Expression

  -- Need to add type variables!!!

  dataDeclaration :: Parser (Decl SourceSpan)
  dataDeclaration = withLoc $ do
    symbol "data"
    name <- constructor
    symbol "="
    types <- typeProduct `sepBy` (lexeme $ char '|')
    return $ DataDeclaration name types

  typeSynonymDeclaration :: Parser (Decl SourceSpan)
  typeSynonymDeclaration = withLoc $ do
    symbol "type"
    aliasee <- typeName
    symbol "="
    alias <- typeName
    return $ TypeSynonymDeclaration aliasee alias

  valueDeclaration :: Parser (Decl SourceSpan)
  valueDeclaration = withLoc $ do
    name <- identifier
    matchers <- (:) <$> fBody <*> (many $ symbol name *> fBody <* scn)
    return $ (uncurry (ValueDeclaration name)) (unzip matchers)
    where fBody = do
                  pat <- pattern
                  symbol "="
                  body <- expression
                  return (pat, body)

  importDeclaration :: Parser (Decl SourceSpan)
  importDeclaration = withLoc $ do
    symbol "import"
    qual <- qualified
    path <- intercalate "." <$> constructor `sepBy` (char '.')
    imports <- mask
    alias <- (optional $ (symbol "as") *> constructor)

    return $ ImportDeclaration qual imports path alias
    where qualified = isJust <$> (optional $ symbol "qualified")
          mask = (try $ do
            cons <- (symbol "hiding" *> return Hiding) <|> (return Only)
            args <- parens $ list identifier
            return $ cons args) <|> (return All)
