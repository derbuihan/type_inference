module Main where

import Parser

main :: IO ()
main =
  let input = "\\x: Int . x \\x: Int . x"
      parsed = parse input
   in do
        putStrLn "Hello, Haskell!"
        print parsed
        putStrLn "Done!"
