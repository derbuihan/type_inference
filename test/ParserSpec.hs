module ParserSpec where

import Parser
import Test.Hspec
import Types

spec :: Spec
spec = do
  describe "tokenizer" specTokenize

specTokenize :: Spec
specTokenize = do
  it "(\\x -> x) y" $ do
    let input = "(\\x -> x) y"
        actual = tokenize input
        expected = [TokenLParen, TokenLam, TokenStr "x", TokenArrow, TokenStr "x", TokenRParen, TokenStr "y", TokenEOF]
    actual `shouldBe` expected
