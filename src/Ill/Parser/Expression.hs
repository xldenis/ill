module Ill.Parser.Expression where

  import Ill.Parser.Lexer

  import Ill.Syntax

  import Text.Megaparsec
  import Text.Megaparsec.Text

  import Control.Comonad.Cofree

  expression :: Parser (Expr SourceSpan)
  expression = body <|> nonBodyExpr

  nonBodyExpr :: Parser (Expr SourceSpan)
  nonBodyExpr = call <|> assign <|> simpleExpr <|> consExpr

  consExpr :: Parser (Expr SourceSpan)
  consExpr = caseE <|> lambda <|> ifE

  simpleExpr :: Parser (Expr SourceSpan)
  simpleExpr = (try $ doubleLit) <|> integerLit <|> rawString <|> var

  body :: Parser (Expr SourceSpan) -- need backtracking?
  body = withLoc $ do
    Body <$> some (nonBodyExpr <* scn)

  assign :: Parser (Expr SourceSpan)
  assign = withLoc $ do
    names <- try $ do
      list identifier <* symbol "="
    values <- list (call <|> simpleExpr <|> consExpr)
    if (length names) /= (length values) then
      fail "Invalid assignment: length mismatch."
    else
      return $ Assign names values

  call :: Parser (Expr SourceSpan)
  call = try $ withLoc $ do
    func <- var <|> consExpr
    args <- parens $ list nonBodyExpr
    return $ Apply func args

  caseE :: Parser (Expr SourceSpan)
  caseE = withLoc $ do
    symbol "case"
    expr <- expression
    symbol "of"
    matchers <- some $ do
      pat <- pattern
      symbol "->"
      expr <- expression
      return (pat, expr)
    return $ Case expr matchers

  lambda :: Parser (Expr SourceSpan)
  lambda = withLoc $ do
    symbol "fn"
    args <- lexeme $ parens . list $ pattern
    body <- body
    return $ Lambda args body

  ifE :: Parser (Expr SourceSpan)
  ifE = withLoc $ do
    symbol "if"
    cond <- expression
    symbol "then" <* scn
    left <- expression
    symbol "else" <* scn
    right <- expression
    symbol "end"

    return $ If cond left right

  integerLit :: Parser (Expr SourceSpan)
  integerLit = withLoc (Literal . Integer <$> integer)

  doubleLit :: Parser (Expr SourceSpan)
  doubleLit = withLoc (Literal . Double <$> double)

  rawString :: Parser (Expr SourceSpan)
  rawString = withLoc (Literal . RawString <$> str)
    where str = squotes (many $ noneOf "'")

  var :: Parser (Expr SourceSpan)
  var = try $ withLoc (Var <$> identifier)

  pattern :: Parser Pattern
  pattern = return Nil
