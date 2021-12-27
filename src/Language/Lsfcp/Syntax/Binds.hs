module Language.Lsfcp.Syntax.Binds where

import RIO

import {-# SOURCE #-} Language.Lsfcp.Syntax.Expr

data Bind
    = FunBind
        { funName :: Text
        , funArgs :: [Pat]
        , funBody :: Expr
        }
    | PatBind
        { patLhs :: Pat
        , patRhs :: Expr
        }
    deriving (Show)