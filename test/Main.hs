module Main (main) where

import SimplyTyped

main :: IO ()
main =
  let -- Example expressions
      expr1 = Lam "x" (App (Var "x") (Var "y"))
   in do
        putStrLn "Test suite not yet implemented."
        print expr1
        print typeenv
