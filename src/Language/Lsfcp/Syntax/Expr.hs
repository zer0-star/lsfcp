module Language.Lsfcp.Syntax.Expr where

import RIO

import Language.Lsfcp.Syntax.Binds

type Pat = Expr

data Expr
  = VarE Text
  | VarPatE Text
  | AppE Expr Expr
  | OpE Expr Text Expr
  | ParE Expr
  | LamE [Pat] Expr
  | LetE [Bind] Expr
  | IfE Expr Expr Expr
  | LitE Lit
  | CaseE Expr [Alt]
  | DoE [Stmt]
  | RecordE [(Text, Expr)]
  | SelectionE Expr Text
  | ProjectionE [Text]
  | UpdateE Expr [(Text, Expr)]
  deriving (Show)

data Lit
  = IntegerL Integer
  | StringL String
  | CharL Char
  deriving (Show)

data Stmt
  = ExprS Expr
  | BindS Pat Expr
  | LetS [Bind]
  deriving (Show)

data Alt = Alt Pat Expr
  deriving (Show)