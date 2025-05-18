module Types where

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
