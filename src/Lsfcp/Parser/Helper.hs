module Lsfcp.Parser.Helper where

import RIO
import qualified RIO.Char as C
import qualified RIO.HashSet as HS
import qualified RIO.Text as T

import Lsfcp.Parser.Core

import qualified RIO.NonEmpty.Partial as NE'
import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

sc :: Parser ()
sc = L.space P.space1 (L.skipLineComment "--") (L.skipBlockCommentNested "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

lchar :: Char -> Parser Char
lchar = lexeme . P.char

identifier :: Parser Text
identifier =
  (P.try . lexeme) do
    o <- P.getOffset
    c <- P.letterChar
    cs <- P.takeWhileP Nothing (\c -> C.isAlphaNum c || c == '_')
    let v = T.cons c cs
    when (v `HS.member` keywords) $
      P.region (P.setErrorOffset o)
        . P.unexpected
        . P.Label
        . NE'.fromList
        $ "reserved keyword `" ++ T.unpack v ++ "`"
    return v
    <?> "identifier"

keyword :: Text -> Parser ()
keyword kw = lexeme . P.try $ do
  P.string kw
  P.notFollowedBy $ P.satisfy (\c -> C.isAlphaNum c || c == '_')

keywords :: HS.HashSet Text
keywords =
  HS.fromList
    [ "let"
    , "in"
    , "case"
    , "of"
    , "if"
    , "then"
    , "else"
    , "data"
    , "type"
    , "where"
    , "import"
    , "module"
    , "as"
    , "hiding"
    , "qualified"
    , "infix"
    , "infixl"
    , "infixr"
    , "deriving"
    , "default"
    , "foreign"
    , "export"
    , "primitive"
    , "type"
    , "prefix"
    ]

reservedOps :: HashSet Text
reservedOps = HS.fromList ["->", "<-", "::", "\\"]

opChars :: [Char]
opChars = ":!$%&*+./<=>?@\\^|-~"

opSet :: HashSet Char
opSet = HS.fromList opChars

opLetter :: Parser Char
opLetter = P.oneOf opChars

operator :: Parser Text
operator =
  P.try $ lexeme do
    o <- P.getOffset
    op <- P.takeWhileP (Just "operator") (`HS.member` opSet)
    when (op `HS.member` reservedOps) $
      P.region (P.setErrorOffset o)
        . P.unexpected
        . P.Label
        . NE'.fromList
        $ "reserved keyword `" ++ T.unpack op ++ "`"
    return op

reservedOp :: Text -> Parser ()
reservedOp op = lexeme . P.try $ do
  P.string op
  P.notFollowedBy opLetter <?> ("end of " ++ show op)

braces :: Parser a -> Parser a
braces = P.between (lchar '{') (lchar '}')

parens :: Parser a -> Parser a
parens = P.between (lchar '(') (lchar ')')

listBlock :: Parser a -> Parser [a]
listBlock p = braces (P.sepEndBy p (some $ lchar ';'))

tightDot :: Parser ()
tightDot = P.try $ P.char '.' >> P.notFollowedBy (void opLetter <|> P.space1)