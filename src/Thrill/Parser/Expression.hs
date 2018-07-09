{-# LANGUAGE OverloadedStrings #-}
module Thrill.Parser.Expression where

import           Thrill.Prelude

import Thrill.Parser.Lexer
import Thrill.Parser.Pattern
import Thrill.Parser.Literal
import Thrill.Syntax

import Text.Megaparsec hiding (some, many)
import Text.Megaparsec.Expr

import Control.Comonad.Cofree
import Control.Monad (when)

import Control.Lens.Extras
import Control.Lens ((^?))
import Data.Text (Text, unpack)

expression :: Parser (Expr' Name SourceSpan)
expression = body <|> nonBodyExpr

nonBodyExpr :: Parser (Expr' Name SourceSpan)
nonBodyExpr = assign <|> fullExpr

fullExpr :: Parser (Expr' Name SourceSpan)
fullExpr = makeExprParser (call <|> parens fullExpr <|> primExpr) opTable
  where primExpr = simpleExpr <|> consExpr

consExpr :: Parser (Expr' Name SourceSpan)
consExpr = caseE <|> lambda <|> ifE

simpleExpr :: Parser (Expr' Name SourceSpan)
simpleExpr = literalE <|> var <|> constructor

body :: Parser (Expr' Name SourceSpan) -- need backtracking?
body = label "body expression" . withLoc $ do
  bodyExps <- nonBodyExpr `sepEndBy1` sep
  when (isJust $ (last bodyExps) ^? _unwrap . _Assign) (fail "blocks must be terminated by non-assignment expression")
  return $ Body bodyExps

assign :: Parser (Expr' Name SourceSpan)
assign = label "assignment" . withLoc $ do
  names <- try $ do
    list identifier <* (symbol "=" <* notFollowedBy (char '='))
  values <- list fullExpr

  when (length names /= length values) $ fail "Invalid assignment: length mismatch."

  return $ Assign names values

call :: Parser (Expr' Name SourceSpan)
call = label "functional invocation" . try $ do
  start <- getPosition
  func <- var <|> constructor <|> parens consExpr
  args <- some $ (,) <$> (parens $ list fullExpr) <*> getPosition
  return $ foldl (f start) func args
  where f startpos func (args, pos) = (SourceSpan startpos pos) :< Apply func args

caseE :: Parser (Expr' Name SourceSpan)
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

lambda :: Parser (Expr' Name SourceSpan)
lambda = label "lambda expression" . withLoc $ do
  symbol "fn"
  args <- lexeme $ parens . list $ pattern
  symbol "=" <* scn
  body <- body
  symbol "end"
  return $ Lambda args body

ifE :: Parser (Expr' Name SourceSpan)
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

var :: Parser (Expr' Name SourceSpan)
var = label "variable" . try $ withLoc (Var <$> identifier)

constructor :: Parser (Expr' Name SourceSpan)
constructor = label "constructor" $ withLoc (Constructor <$> lexeme capitalized)

opTable :: [[Operator Parser (Expr' Name SourceSpan)]]
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

binary :: Parser Text -> Operator Parser (Expr' Name SourceSpan)
binary op = InfixL $ do
  op <- withLoc $ Var . unpack <$> op
  return $ \a b -> SourceSpan (begin $ extract a) (end $ extract b) :< (BinOp op a b)

