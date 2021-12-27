module Lsfcp.Parser.Expr where

import RIO
import RIO.List
import qualified RIO.Text as T

import Language.Lsfcp.Syntax
import Lsfcp.Parser.Core
import Lsfcp.Parser.Helper

import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

-- | Parse a integer literal.
integerLit :: Parser Lit
integerLit = P.try $ lexeme $ IntegerL <$> L.signed (return ()) (binary <|> octal <|> hexadecimal <|> decimal)
 where
  binary = P.try (P.string' "0b") *> L.binary
  octal = P.try (P.string' "0o") *> L.octal
  hexadecimal = P.try (P.string' "0x") *> L.hexadecimal
  decimal = L.decimal

stringLit :: Parser Lit
stringLit = lexeme $ StringL <$> (P.char '"' *> P.manyTill L.charLiteral (P.char '"' <?> "closing quote"))

verbatimStringLit :: Parser Lit
verbatimStringLit =
  lexeme $ do
    P.try $ P.string "\"\"\""
    s <- P.manyTill P.anySingle $ P.try (P.string "\"\"\"" <?> "closing triple quotes")
    qs <- T.unpack <$> P.takeWhileP Nothing (== '"')
    return $ StringL $ s <> qs

charLit :: Parser Lit
charLit = lexeme $ CharL <$> P.between (P.char '\'') (P.char '\'') L.charLiteral

literal :: Parser Expr
literal = LitE <$> (integerLit <|> verbatimStringLit <|> stringLit <|> charLit) <?> "literal"

-- | Parse a function declaration
funDec :: Parser Bind
funDec = do
  name <- identifier
  args <- P.many expr2
  lchar '='
  body <- expr
  return $ FunBind name args body

-- | Parse a pattern declaration
patDec :: Parser Bind
patDec = do
  pat <- expr2
  lchar '='
  body <- expr
  return $ PatBind pat body

decl :: Parser Bind
decl = funDec <|> patDec

var :: Parser Expr
var = VarE <$> identifier

varPat :: Parser Expr
varPat = VarPatE <$> (P.char '#' *> identifier)

field :: Parser (Text, Expr)
field = do
  name <- identifier
  lchar '='
  e <- expr
  return (name, e)

record :: Parser Expr
record = do
  lchar '{'
  fields <- P.sepEndBy field (lchar ',')
  lchar '}'
  return $ RecordE fields

let_ :: Parser Expr
let_ = do
  keyword "let"
  binds <- listBlock decl
  keyword "in"
  body <- expr
  return $ LetE binds body

if_ :: Parser Expr
if_ = do
  keyword "if"
  cond <- expr
  keyword "then"
  thenBranch <- expr
  keyword "else"
  elseBranch <- expr
  return $ IfE cond thenBranch elseBranch

do_ :: Parser Expr
do_ = DoE <$> (keyword "do" *> listBlock (letS <|> bindS <|> exprS))
 where
  letS = do
    keyword "let"
    ds <- listBlock decl
    ( do
        keyword "in"
        e <- expr
        return $ ExprS (LetE ds e)
      )
      <|> return (LetS ds)

  exprS = ExprS <$> expr

  bindS = P.try do
    pat <- expr
    reservedOp "<-"
    rhs <- expr
    return $ BindS pat rhs

selection :: Parser Expr
selection = do
  e <- expr1
  ss <- many (P.try $ tightDot *> (identifier <?> "field name"))
  return $ foldl SelectionE e ss

projection :: Parser Expr
projection = P.try $ lexeme $ ProjectionE <$> parens (some $ tightDot *> identifier)

update :: Parser Expr
update = do
  e <- expr2
  P.try
    ( tightDot
        *> braces do
          fields <- P.sepEndBy field (lchar ',')
          return $ UpdateE e fields
    )
    <|> return e

paren :: Parser Expr
paren = ParE <$> parens expr

expr1 :: Parser Expr
expr1 = record <|> literal <|> projection <|> paren <|> var <|> varPat

expr2 :: Parser Expr
expr2 = selection

expr3 :: Parser Expr
expr3 = update

expr4 :: Parser Expr
expr4 = if_ <|> do_ <|> let_ <|> expr3

aexpr :: Parser Expr
aexpr = expr4

fexpr :: Parser Expr
fexpr = do
  e <- aexpr
  es <- many aexpr
  return $ foldl AppE e es

opexpr :: Parser Expr
opexpr = do
  e <- fexpr
  oes <- many ((,) <$> operator <*> fexpr)
  return $ foldl (fmap uncurry OpE) e oes

expr :: Parser Expr
expr = opexpr

program :: Parser [Bind]
program = P.sepEndBy decl (some $ lchar ';')