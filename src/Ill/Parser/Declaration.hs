module Ill.Parser.Declaration (declaration) where

  import Data.List (intercalate)
  import Data.Maybe

  import Text.Megaparsec.Text
  import Text.Megaparsec

  import Ill.Syntax
  import Ill.Parser.Lexer
  import Ill.Parser.Type
  import Ill.Parser.Expression


  declaration :: Parser (Decl SourceSpan)
  declaration = dataDeclaration <|> typeSynonymDeclaration <|> importDeclaration <|> valueDeclaration <|> signatureDeclaration

  -- Need to add type variables!!!

  dataDeclaration :: Parser (Decl SourceSpan)
  dataDeclaration = withLoc $ do
    symbol "data"
    name <- upperIdent
    symbol "="
    types <- typeProduct `sepBy` (lexeme $ char '|')
    return $ Data name types

  typeSynonymDeclaration :: Parser (Decl SourceSpan)
  typeSynonymDeclaration = withLoc $ do
    symbol "type"
    alias <- typeProduct
    symbol "="
    aliasee <- typeProduct
    return $ TypeSynonym alias aliasee

  signatureDeclaration :: Parser (Decl SourceSpan)
  signatureDeclaration = withLoc $ do
    ident <- try $ identifier
    symbol "::"
    tp <- typeExp
    return $ Signature ident tp

  -- | TODO: Argument pattern matching?
  valueDeclaration :: Parser (Decl SourceSpan)
  valueDeclaration = withLoc $ do
    symbol "fn"
    name <- identifier
    args <- parens $ list pattern
    ret <- optional $ symbol ":" *> typeExp
    scn
    body <- body
    scn
    symbol "end"
    return $ Value name ret args [body]

  importDeclaration :: Parser (Decl SourceSpan)
  importDeclaration = withLoc $ do
    symbol "import"
    qual <- qualified
    path <- intercalate "." <$> (lexeme $ capitalized `sepBy` (char '.'))
    alias <- if qual then Just <$> alias else optional alias
    imports <- mask

    return $ Import qual imports path alias
    where qualified = isJust <$> (optional $ symbol "qualified")
          alias = symbol "as" *> upperIdent
          mask = (try $ do
            cons <- (symbol "hiding" *> return Hiding) <|> (return Only)
            args <- parens $ list identifier
            return $ cons args) <|> (return All)
