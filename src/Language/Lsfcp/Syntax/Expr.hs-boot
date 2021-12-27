{-# LANGUAGE RoleAnnotations #-}

module Language.Lsfcp.Syntax.Expr where

import RIO (Show)

type Pat = Expr

data Expr

instance Show Expr