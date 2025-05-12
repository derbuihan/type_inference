module Main where

import qualified SimplyTyped (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  SimplyTyped.someFunc
