module ExprSpec where

import Expr
import Test.Hspec

expr1 = Expr (Val 2) (Expr (Val 3) (Val 4))
ops1 = [M, A]

expr2 = Expr (Expr (Val 5) (Val 13)) (Expr (Val (-2)) (Expr (Val 1) (Expr (Val 4) (Val (-3)))))
ops2 = [D, A, A, M, A]

expr3 = Expr (Val 5) (Expr (Val 6) (Val 7))
ops3 = [A, M]

expr4 = Expr (Val 5) (Val (-2))
ops4 = [M]

expr5 = Expr (Val (-2)) (Val 5)
ops5 = [M]


spec :: Spec
spec = do
  describe ("Expression 1 " ++ show expr1 ++ " $ " ++ show ops1) $ do
    it "evaluated correctly" $ do
      (eval expr1 ops1) `shouldBe` 14.0
    it "formated correctly" $ do
      (format expr1 ops1) `shouldBe` "2*(3+4)"
  describe ("Expression 2 " ++ show expr2 ++ " $ " ++ show ops2) $ do
    it "evaluated correctly" $ do
      (eval expr2 ops2) `shouldBe` (-18.0)
    it "formated correctly" $ do
      (format expr2 ops2) `shouldBe` "(5+13)/(-2+1*(4-3))"
  describe ("Expression 3 " ++ show expr3 ++ " $ " ++ show ops3) $ do
    it "evaluated correctly" $ do
      (eval expr3 ops3) `shouldBe` 47.0
    it "formated correctly" $ do
      (format expr3 ops3) `shouldBe` "5+6*7"
  describe ("Expression 4 " ++ show expr4 ++ " $ " ++ show ops4) $ do
    it "evaluated correctly" $ do
      (eval expr4 ops4) `shouldBe` (-10.0)
    it "formated correctly" $ do
      (format expr4 ops4) `shouldBe` "5*(-2)"
  describe ("Expression 5 " ++ show expr5 ++ " $ " ++ show ops5) $ do
    it "evaluated correctly" $ do
      (eval expr5 ops5) `shouldBe` (-10.0)
    it "formated correctly" $ do
      (format expr5 ops5) `shouldBe` "(-2)*5"
