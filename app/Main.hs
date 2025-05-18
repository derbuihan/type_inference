module Main where

import Inference
import Parser
import Types

main :: IO ()
main =
  let input = "\\x: Int -> Int. x"
      parsed = parse input
      env = fromList [("y", TInt)]
      typed = infer env parsed
   in do
        putStrLn "Hello, Haskell!"
        print parsed
        print typed
        putStrLn "Done!"
