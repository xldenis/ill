module Ill.Parser.Expression where

  import Ill.Parser.Lexer

  import Ill.Syntax

  import Text.Megaparsec
  import Text.Megaparsec.Text

  import Control.Comonad.Cofree

  expression :: Parser (Expr SourceSpan)
  expression = integerLit

  assign :: Parser (Expr SourceSpan)
  assign = withLoc $ do
    names <- list identifier
    symbol "="
    values <- list expression
    if (length names) /= (length values) then
      fail "Invalid assignment: length mismatch."
    else
      return $ Assign names values

  call :: Parser (Expr SourceSpan)
  call = withLoc $ do
    func <- expression
    args <- parens $ list expression
    return $ Apply func args

  caseE :: Parser (Expr SourceSpan)
  caseE = withLoc $ do
    symbol "case"
    expr <- expression
    symbol "of"
    matchers <- many $ do
      pat <- pattern
      symbol "->"
      expr <- expression
      return (pat, expr)
    return $ Case expr matchers

  lambda :: Parser (Expr SourceSpan)
  lambda = withLoc $ do
    symbol "fn"
    args <- parens . list $ pattern
    body <- expression
    return $ Lambda args body

  ifE :: Parser (Expr SourceSpan)
  ifE = withLoc $ do
    symbol "if"
    cond <- expression
    symbol "then"
    left <- expression
    symbol "else"
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
  var = withLoc (Var <$> identifier)

  pattern :: Parser Pattern
  pattern = return Nil
