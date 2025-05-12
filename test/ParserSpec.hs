module ParserSpec where

import Parser
import Test.Hspec
import Types

spec :: Spec
spec = do
  describe "tokenize" specTokenize
  describe "parse" specParse

specTokenize :: Spec
specTokenize = do
  it "tokenize (\\x -> x) y" $ do
    let input = "(\\x -> x) y"
        actual = tokenize input
        expected = [TokenLParen, TokenLam, TokenStr "x", TokenArrow, TokenStr "x", TokenRParen, TokenStr "y", TokenEOF]
    actual `shouldBe` expected

specParse :: Spec
specParse = do
  it "parse (\\x -> x) y" $ do
    let input = "(\\x -> x) y"
        actual = parse input
        expected = App (Lam "x" (Var "x")) (Var "y")
    actual `shouldBe` expected

  it "parse \\x -> \\y -> x y" $ do
    let input = "\\x -> \\y -> x y"
        actual = parse input
        expected = Lam "x" (Lam "y" (App (Var "x") (Var "y")))
    actual `shouldBe` expected

  it "parse \\x -> x \\y -> y" $ do
    let input = "\\x -> x \\y -> y"
        actual = parse input
        expected = Lam "x" (App (Var "x") (Lam "y" (Var "y")))
    actual `shouldBe` expected

  it "parse (\\x -> x) (\\y -> y)" $ do
    let input = "(\\x -> x) (\\y -> y)"
        actual = parse input
        expected = App (Lam "x" (Var "x")) (Lam "y" (Var "y"))
    actual `shouldBe` expected
