module InferenceSpec where

import Inference
import Parser
import Test.Hspec
import Types

spec :: Spec
spec = do
  describe "STLC" specInference

specInference :: Spec
specInference = do
  it "infer (\\x : Int . x)" $ do
    let input = "(\\x : Int . x)"
        actual = infer emptyEnv (parse input)
        expected = TFun TInt TInt
    actual `shouldBe` expected

  it "infer (\\x : Int . \\y : Int . x y)" $ do
    let input = "(\\x : Int . \\y : Int . x)"
        actual = infer emptyEnv (parse input)
        expected = TFun TInt (TFun TInt TInt)
    actual `shouldBe` expected

  it "infer (\\x : Int . x) y" $ do
    let input = "(\\x : Int . x) y"
        actual = infer (fromList [("y", TInt)]) (parse input)
        expected = TInt
    actual `shouldBe` expected

  it "infer (\\x : Int -> Int . x) y" $ do
    let input = "(\\x : Int -> Int . x) y"
        actual = infer (fromList [("y", TFun TInt TInt)]) (parse input)
        expected = TFun TInt TInt
    actual `shouldBe` expected
