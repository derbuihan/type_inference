module Types where

import Data.Map

data Expr
  = Var String
  | Lam String Expr
  | App Expr Expr
  deriving (Show, Eq)

data Type
  = TInt
  | TBool
  | TVar String
  | TFun Type Type
  deriving (Show, Eq)

type TypeEnv = Map String Type

data Token
  = TokenStr String -- x, y, etc.
  | TokenLam -- \
  | TokenDot -- .
  | TokenLParen -- (
  | TokenRParen -- )
  | TokenEOF -- end of file
  deriving (Show, Eq)
