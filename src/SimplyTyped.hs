module SimplyTyped where

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

typeenv = fromList [("x", TVar "a"), ("y", TVar "b")]
