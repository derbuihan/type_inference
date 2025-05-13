module Types where

import Data.Map

data Expr
  = Var String
  | Lam String Type Expr
  | App Expr Expr
  deriving (Show, Eq)

data Type
  = TInt
  | TBool
  | TFun Type Type
  deriving (Show, Eq)

type TypeEnv = Map String Type

data Token
  = TokenStr String -- x, y, etc.
  | TokenLam -- \
  | TokenLParen -- (
  | TokenRParen -- )
  | TokenColon -- :
  | TokenDot -- .
  | TokenArrow -- ->
  | TokenTInt -- Int
  | TokenTBool -- Bool
  | TokenEOF -- end of file
  deriving (Show, Eq)
