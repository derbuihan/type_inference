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
tokenize (':' : xs) = TokenColon : tokenize xs
tokenize ('.' : xs) = TokenDot : tokenize xs
tokenize ('-' : '>' : xs) = TokenArrow : tokenize xs
tokenize (x : xs)
  | isAlpha x =
      let (keyword, rest) = span isAlphaNum (x : xs)
       in convertKeyword keyword : tokenize rest
  | otherwise = error $ "Unknown character: " ++ [x]

convertKeyword :: String -> Token
convertKeyword "Int" = TokenTInt
convertKeyword "int" = TokenTInt
convertKeyword "Bool" = TokenTBool
convertKeyword "bool" = TokenTBool
convertKeyword str = TokenStr str

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
parseLam (TokenLam : TokenStr var : TokenColon : rest) =
  let (type_, rest_) = parseType rest
   in case rest_ of
        (TokenDot : rest__) ->
          let (body, rest___) = parse_ rest__
           in (Lam var type_ body, rest___)
        _ -> error $ "Expected colon after variable, got: " ++ show rest_
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

parseType :: Parser Type
parseType tokens =
  let (type1, rest) = parseTypeParen tokens
   in case rest of
        (TokenArrow : _) ->
          let (type2, rest_) = parseType rest
           in (TFun type1 type2, rest_)
        _ -> (type1, rest)

parseTypeParen :: Parser Type
parseTypeParen (TokenLParen : rest) =
  let (type1, rest_) = parseType rest
   in case rest_ of
        (TokenRParen : rest__) -> (type1, rest__)
        _ -> error $ "Expected closing parenthesis or arrow, got: " ++ show rest_
parseTypeParen tokens = parseTypePrimitive tokens

parseTypePrimitive :: Parser Type
parseTypePrimitive (TokenTInt : rest) = (TInt, rest)
parseTypePrimitive (TokenTBool : rest) = (TBool, rest)
parseTypePrimitive tokens = error $ "Expected type, got: " ++ show tokens
