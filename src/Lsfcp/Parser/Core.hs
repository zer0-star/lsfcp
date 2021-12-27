module Lsfcp.Parser.Core where

import RIO

import qualified Text.Megaparsec as P

-- import qualified Text.Megaparsec.Char as P
-- import qualified Text.Megaparsec.Char.Lexer as L

type Parser = P.Parsec Void Text