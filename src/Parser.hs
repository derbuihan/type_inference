{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

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
tokenize ('.' : xs) = TokenDot : tokenize xs
tokenize (x : xs)
  | isAlpha x =
      let (keyword, rest) = span isAlphaNum (x : xs)
       in TokenStr keyword : tokenize rest
  | otherwise = error $ "Unknown character: " ++ [x]

-- parse

parse :: String -> Expr
parse input =
  let tokens = tokenize input
      (parsed, rest) = parse_ tokens
   in case rest of
        [TokenEOF] -> parsed
        _ -> error $ "Parsing error, remaining tokens: " ++ show rest

type Parser a = [Token] -> (a, [Token])

parse_ :: Parser Expr
parse_ = parseApp

parseApp :: Parser Expr
parseApp tokens =
  let (expr, rest) = parseLam tokens
   in parseAppChain expr rest

parseAppChain :: Expr -> Parser Expr
parseAppChain left tokens@(TokenRParen : _) = (left, tokens)
parseAppChain left tokens@[TokenEOF] = (left, tokens)
parseAppChain left tokens =
  case parseLam tokens of
    (right, rest) -> parseAppChain (App left right) rest
    _ -> (left, tokens)

parseLam :: Parser Expr
parseLam (TokenLam : TokenStr var : TokenDot : rest) =
  let (body, rest_) = parse_ rest
   in (Lam var body, rest_)
parseLam tokens = parseParen tokens

parseParen :: Parser Expr
parseParen (TokenLParen : rest) =
  let (body, rest_) = parse_ rest
   in case rest_ of
        (TokenRParen : rest__) -> (body, rest__)
        _ -> error $ "Expected closing parenthesis " ++ show rest_
parseParen tokens = parseVar tokens

parseVar :: Parser Expr
parseVar (TokenStr var : rest) = (Var var, rest)
parseVar tokens = error $ "Expected variable, got: " ++ show tokens
