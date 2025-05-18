module Inference where

import Data.Map (Map, empty, insert, lookup)
import Types
import Prelude hiding (lookup)

type TypeEnv = Map String Type

emptyEnv :: TypeEnv
emptyEnv = empty

fromList :: [(String, Type)] -> TypeEnv
fromList = foldr (\(k, v) acc -> insert k v acc) empty

infer :: TypeEnv -> Expr -> Type
infer env (Var x) =
  case lookup x env of
    Just t -> t
    Nothing -> error $ "Unbound variable: " ++ x
infer env (Lam x t body) =
  let newEnv = insert x t env
      bodyType = infer newEnv body
   in TFun t bodyType
infer env (App func arg) =
  let funcType = infer env func
      argType = infer env arg
   in case funcType of
        TFun t1 t2
          | t1 == argType -> t2
          | otherwise -> error $ "Type mismatch: expected " ++ show t1 ++ ", got " ++ show argType
        _ -> error $ "Cannot apply non-function type: " ++ show funcType
