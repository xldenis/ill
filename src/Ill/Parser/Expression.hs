module Ill.Parser.Expression where

import Ill.Parser.Lexer
import Ill.Parser.Pattern

import Ill.Syntax

import Text.Megaparsec
import Text.Megaparsec.Text
import Text.Megaparsec.Expr

import Control.Comonad.Cofree
import Control.Comonad
import Control.Monad (when)

expression :: Parser (Expr SourceSpan)
expression = body <|> nonBodyExpr

nonBodyExpr :: Parser (Expr SourceSpan)
nonBodyExpr = assign <|> fullExpr

fullExpr :: Parser (Expr SourceSpan)
fullExpr = makeExprParser (call <|> parens primExpr <|> primExpr) opTable
  where primExpr = simpleExpr <|> consExpr

consExpr :: Parser (Expr SourceSpan)
consExpr = caseE <|> lambda <|> ifE

simpleExpr :: Parser (Expr SourceSpan)
simpleExpr = (try $ doubleLit) <|> integerLit <|> rawString <|> var <|> constructor

body :: Parser (Expr SourceSpan) -- need backtracking?
body = withLoc $ do
  Body <$> some (nonBodyExpr <* scn)

assign :: Parser (Expr SourceSpan)
assign = withLoc $ do
  names <- try $ do
    list identifier <* symbol "="
  values <- list fullExpr

  when (length names /= length values) $ fail "Invalid assignment: length mismatch."

  return $ Assign names values

call :: Parser (Expr SourceSpan)
call = try $ do
  start <- getPosition
  func <- var <|> constructor <|> parens consExpr
  args <- some $ (,) <$> (parens $ list fullExpr) <*> getPosition
  return $ foldl (f start) func args
  where f startpos func (args, pos) =(SourceSpan startpos pos)  :< Apply func args

caseE :: Parser (Expr SourceSpan)
caseE = withLoc $ do
  symbol "case"
  expr <- fullExpr
  symbol "of"
  scn
  matchers <- some $ do
    pat <- try pattern
    symbol "->"
    expr <- expression
    scn
    return (pat, expr)
  symbol "end"
  return $ Case expr [] -- matchers

lambda :: Parser (Expr SourceSpan)
lambda = withLoc $ do
  symbol "fn"
  args <- lexeme $ parens . list $ pattern
  symbol "=" <* scn
  body <- body
  symbol "end"
  return $ Lambda args body

ifE :: Parser (Expr SourceSpan)
ifE = withLoc $ do
  symbol "if"
  cond <- fullExpr
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

constructor :: Parser (Expr SourceSpan)
constructor = withLoc (Constructor <$> lexeme capitalized)

opTable :: [[Operator Parser (Expr SourceSpan)]]
opTable = [ [ binary "*"
            , binary "/"
            , binary "&&"]
          , [ binary "+"
            , binary "-"
            , binary "||"]
          , [ binary ">"
            , binary "<"]
          ]

binary :: String -> Operator Parser (Expr SourceSpan)
binary op = InfixL $ do
  op <- withLoc $ Var <$> (symbol op)
  return $ \a b -> SourceSpan (begin $ extract a) (end $ extract b) :< (BinOp op a b)

