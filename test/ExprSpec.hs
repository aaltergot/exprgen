{-# LANGUAGE OverloadedStrings #-}

module ExprSpec where

import           Expr
import           Test.Hspec

eshow :: (Show a) => (Expr a, [Op]) -> String
eshow (e, ops) = show e ++ " $ " ++ show ops

ops1 = [A, A, A]
ops2 = [D, D, D]
ops3 = [D, D, A]

expr1 = (Val 1, [])
expr2 = (Val (-1), [])
expr3 = (Expr (Val 1) (Val 1), [A])
expr4 = (Expr (Val (-1)) (Val 1), [A])
expr5 = (Expr (Val 1) (Val (-1)), [A])
expr6 = (Expr (Val (-1)) (Val (-1)), [A])
expr7 = (Expr (Val 5) (Val (-2)), [M])
expr8 = (Expr (Val (-2)) (Val 5), [M])
expr9 = (Expr (Val 2) (Expr (Val 3) (Val 4)), [M, A])
expr10 = (Expr (Expr (Val 5) (Val 6)) (Val 7), [M, A])
expr11 =
  ( Expr (Expr (Val 5) (Val 13)) (Expr (Val (-2)) (Expr (Val 1) (Expr (Val 4) (Val (-3)))))
  , [D, A, A, M, A] )

spec :: Spec
spec = do

  describe ("Ops1: " ++ show ops1) $ do
    it "succeeded correctly" $ do (succOps ops1) `shouldBe` [M, A, A]

  describe ("Ops2: " ++ show ops2) $ do
    it "succeeded correctly" $ do (succOps ops2) `shouldBe` [A, A, A]

  describe ("Ops3: " ++ show ops3) $ do
    it "succeeded correctly" $ do (succOps ops3) `shouldBe` [A, A, M]

  describe ("Expr1: " ++ eshow expr1) $ do
    let (e, ops) = expr1
    it "evaluated correctly" $ do (eval e ops) `shouldBe` 1.0
    it "formated correctly" $ do (format e ops) `shouldBe` "1"

  describe ("Expr2: " ++ eshow expr2) $ do
    let (e, ops) = expr2
    it "evaluated correctly" $ do (eval e ops) `shouldBe` (-1.0)
    it "formated correctly" $ do (format e ops) `shouldBe` "-1"

  describe ("Expr3: " ++ eshow expr3) $ do
    let (e, ops) = expr3
    it "evaluated correctly" $ do (eval e ops) `shouldBe` 2.0
    it "formated correctly" $ do (format e ops) `shouldBe` "1+1"

  describe ("Expr4: " ++ eshow expr4) $ do
    let (e, ops) = expr4
    it "evaluated correctly" $ do (eval e ops) `shouldBe` 0.0
    it "formated correctly" $ do (format e ops) `shouldBe` "-1+1"

  describe ("Expr5: " ++ eshow expr5) $ do
    let (e, ops) = expr5
    it "evaluated correctly" $ do (eval e ops) `shouldBe` 0.0
    it "formated correctly" $ do (format e ops) `shouldBe` "1-1"

  describe ("Expr6: " ++ eshow expr6) $ do
    let (e, ops) = expr6
    it "evaluated correctly" $ do (eval e ops) `shouldBe` (-2.0)
    it "formated correctly" $ do (format e ops) `shouldBe` "-1-1"

  describe ("Expr7: " ++ eshow expr7) $ do
    let (e, ops) = expr7
    it "evaluated correctly" $ do (eval e ops) `shouldBe` (-10.0)
    it "formated correctly" $ do (format e ops) `shouldBe` "5*(-2)"

  describe ("Expr8: " ++ eshow expr8) $ do
    let (e, ops) = expr8
    it "evaluated correctly" $ do (eval e ops) `shouldBe` (-10.0)
    it "formated correctly" $ do (format e ops) `shouldBe` "(-2)*5"

  describe ("Expr9: " ++ eshow expr9) $ do
    let (e, ops) = expr9
    it "evaluated correctly" $ do (eval e ops) `shouldBe` 14.0
    it "formated correctly" $ do (format e ops) `shouldBe` "2*(3+4)"

  describe ("Expr10: " ++ eshow expr10) $ do
    let (e, ops) = expr10
    it "evaluated correctly" $ do (eval e ops) `shouldBe` 77.0
    it "formated correctly" $ do (format e ops) `shouldBe` "(5+6)*7"

  describe ("Expr11: " ++ eshow expr11) $ do
    let (e, ops) = expr11
    it "evaluated correctly" $ do (eval e ops) `shouldBe` (-18.0)
    it "formated correctly" $ do (format e ops) `shouldBe` "(5+13)/(-2+1*(4-3))"
