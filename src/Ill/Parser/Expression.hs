{-# LANGUAGE OverloadedStrings #-}
module Ill.Parser.Expression where

import Ill.Parser.Lexer
import Ill.Parser.Pattern
import Ill.Parser.Literal
import Ill.Syntax

import Text.Megaparsec
import Text.Megaparsec.Expr

import Control.Comonad.Cofree
import Control.Comonad
import Control.Monad (when)

import Control.Lens.Extras
import Control.Lens ((^?))
import Data.Function ((&))
import Data.Maybe
import Data.Text (Text, unpack)

expression :: Parser (Expr SourceSpan)
expression = body <|> nonBodyExpr

nonBodyExpr :: Parser (Expr SourceSpan)
nonBodyExpr = assign <|> fullExpr

fullExpr :: Parser (Expr SourceSpan)
fullExpr = makeExprParser (call <|> parens fullExpr <|> primExpr) opTable
  where primExpr = simpleExpr <|> consExpr

consExpr :: Parser (Expr SourceSpan)
consExpr = caseE <|> lambda <|> ifE

simpleExpr :: Parser (Expr SourceSpan)
simpleExpr = literalE <|> var <|> constructor

body :: Parser (Expr SourceSpan) -- need backtracking?
body = label "body expression" . withLoc $ do
  bodyExps <- nonBodyExpr `sepEndBy1` sep
  when (isJust $ (last bodyExps) ^? _unwrap . _Assign) (fail "blocks must be terminated by non-assignment expression")
  return $ Body bodyExps

assign :: Parser (Expr SourceSpan)
assign = label "assignment" . withLoc $ do
  names <- try $ do
    list identifier <* symbol "="
  values <- list fullExpr

  when (length names /= length values) $ fail "Invalid assignment: length mismatch."

  return $ Assign names values

call :: Parser (Expr SourceSpan)
call = label "functional invocation" . try $ do
  start <- getPosition
  func <- var <|> constructor <|> parens consExpr
  args <- some $ (,) <$> (parens $ list fullExpr) <*> getPosition
  return $ foldl (f start) func args
  where f startpos func (args, pos) = (SourceSpan startpos pos) :< Apply func args

caseE :: Parser (Expr SourceSpan)
caseE = label "case expression" . withLoc $ do
  symbol "case"
  expr <- fullExpr
  symbol "of"
  scn
  matchers <- some $ do
    symbol "when"
    pat  <- pattern
    symbol ":"
    expr <- body
    scn
    return (pat, expr)
  symbol "end"
  return $ Case expr matchers -- matchers

lambda :: Parser (Expr SourceSpan)
lambda = label "lambda expression" . withLoc $ do
  symbol "fn"
  args <- lexeme $ parens . list $ pattern
  symbol "=" <* scn
  body <- body
  symbol "end"
  return $ Lambda args body

ifE :: Parser (Expr SourceSpan)
ifE = label "if expression" . withLoc $ do
  symbol "if"
  cond <- label "condition" fullExpr
  label "then" $ symbol "then" <* scn
  left <- expression
  label "else-branch" $ symbol "else" <* scn
  right <- expression
  symbol "end"

  return $ If cond left right

literalE = label "literal" $ withLoc (Literal <$> literal)

var :: Parser (Expr SourceSpan)
var = label "variable" . try $ withLoc (Var <$> identifier)

constructor :: Parser (Expr SourceSpan)
constructor = label "constructor" $ withLoc (Constructor <$> lexeme capitalized)

opTable :: [[Operator Parser (Expr SourceSpan)]]
opTable = [ [ binary $ symbol "*"
            , binary $ symbol "/"
            , binary $ symbol "&&"
            ]
          , [ binary $ symbol "+"
            , binary $ symbol "-"
            , binary $ symbol "||"
            ]
          , [ binary $ try . lexeme $ string ">" <* notFollowedBy (char '=')
            , binary $ try . lexeme $ string "<" <* notFollowedBy (char '=')
            , binary $ symbol ">="
            , binary $ symbol "<="
            , binary $ symbol "=="
            ]
          ]

binary :: Parser Text -> Operator Parser (Expr SourceSpan)
binary op = InfixL $ do
  op <- withLoc $ Var . unpack <$> op
  return $ \a b -> SourceSpan (begin $ extract a) (end $ extract b) :< (BinOp op a b)

