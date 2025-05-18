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
  it "tokenize (\\x : Int . x) y" $ do
    let input = "(\\x : Int . x) y"
        actual = tokenize input
        expected = [TokenLParen, TokenLam, TokenStr "x", TokenColon, TokenTInt, TokenDot, TokenStr "x", TokenRParen, TokenStr "y", TokenEOF]
    actual `shouldBe` expected

specParse :: Spec
specParse = do
  it "parse (\\x : Int . x) y" $ do
    let input = "(\\x : Int . x) y"
        actual = parse input
        expected = App (Lam "x" TInt (Var "x")) (Var "y")
    actual `shouldBe` expected

  it "parse \\x  : Int . \\y  : Int . x y" $ do
    let input = "\\x : Int . \\y : Int . x y"
        actual = parse input
        expected = Lam "x" TInt (Lam "y" TInt (App (Var "x") (Var "y")))
    actual `shouldBe` expected

  it "parse \\x : Int . x \\y : Int . y" $ do
    let input = "\\x : Int . x \\y : Int . y"
        actual = parse input
        expected = Lam "x" TInt (App (Var "x") (Lam "y" TInt (Var "y")))
    actual `shouldBe` expected

  it "parse (\\x : Int . x) (\\y : Int . y)" $ do
    let input = "(\\x : Int . x) (\\y : Int . y)"
        actual = parse input
        expected = App (Lam "x" TInt (Var "x")) (Lam "y" TInt (Var "y"))
    actual `shouldBe` expected

  it "parse \\x : Int -> Int . x" $ do
    let input = "\\x : Int -> Int . x"
        actual = parse input
        expected = Lam "x" (TFun TInt TInt) (Var "x")
    actual `shouldBe` expected
