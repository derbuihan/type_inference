module Main where

import Parser

main :: IO ()
main =
  let input = "(\\x -> x) y"
      tokens = tokenize input
   in do
        putStrLn "Hello, Haskell!"
        print tokens
