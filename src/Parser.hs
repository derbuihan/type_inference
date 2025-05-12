module Parser where

import Data.Char
import Types

-- tokenize

tokenize :: String -> [Token]
tokenize [] = [TokenEOF]
tokenize (' ' : xs) = tokenize xs
tokenize ('\\' : xs) = TokenLam : tokenize xs
tokenize ('(' : xs) = TokenLParen : tokenize xs
tokenize (')' : xs) = TokenRParen : tokenize xs
tokenize ('-' : '>' : xs) = TokenArrow : tokenize xs
tokenize (x : xs)
  | isAlpha x =
      let (keyword, rest) = span isAlphaNum (x : xs)
       in TokenStr keyword : tokenize rest
  | otherwise = error $ "Unknown character: " ++ [x]
