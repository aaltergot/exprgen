{-# LANGUAGE OverloadedStrings #-}

module ExprGen.ExprSpec where

import Control.Monad.Except (runExcept)
import Test.Hspec

import ExprGen.Expr

eshow :: (Show a) => (Expr a, [Op]) -> String
eshow (e, ops) = show e ++ " $ " ++ show ops

ops1, ops2, ops3 :: [Op]

ops1 = [A, A, A]
ops2 = [D, D, D]
ops3 = [D, D, A]

expr1, expr2, expr3, expr4, expr5, expr6, expr7, expr8, expr9, expr10 :: (Expr Integer, [Op])
expr11, expr12, expr13 :: (Expr Integer, [Op])

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
expr12 = (Expr (Val 1) (Expr (Val (-1)) (Val 1)), [A, A])
expr13 = (Expr (Val 1) (Expr (Val (-1)) (Val 1)), [A])

spec :: Spec
spec = do

  describe ("Ops1: " ++ show ops1) $
    it "succeeded correctly" $ succOps ops1 `shouldBe` [M, A, A]

  describe ("Ops2: " ++ show ops2) $
    it "succeeded correctly" $ succOps ops2 `shouldBe` [A, A, A]

  describe ("Ops3: " ++ show ops3) $
    it "succeeded correctly" $ succOps ops3 `shouldBe` [A, A, M]

  describe ("Expr1: " ++ eshow expr1) $ do
    let (e, ops) = expr1
    it "evaluated correctly" $ runExcept (eval e ops) `shouldBe` Right 1.0
    it "formated correctly" $ runExcept (format e ops) `shouldBe` Right "1"

  describe ("Expr2: " ++ eshow expr2) $ do
    let (e, ops) = expr2
    it "evaluated correctly" $ runExcept (eval e ops) `shouldBe` Right (-1.0)
    it "formated correctly" $ runExcept (format e ops) `shouldBe` Right "-1"

  describe ("Expr3: " ++ eshow expr3) $ do
    let (e, ops) = expr3
    it "evaluated correctly" $ runExcept (eval e ops) `shouldBe` Right 2.0
    it "formated correctly" $ runExcept (format e ops) `shouldBe` Right "1+1"

  describe ("Expr4: " ++ eshow expr4) $ do
    let (e, ops) = expr4
    it "evaluated correctly" $ runExcept (eval e ops) `shouldBe` Right 0.0
    it "formated correctly" $ runExcept (format e ops) `shouldBe` Right "-1+1"

  describe ("Expr5: " ++ eshow expr5) $ do
    let (e, ops) = expr5
    it "evaluated correctly" $ runExcept (eval e ops) `shouldBe` Right 0.0
    it "formated correctly" $ runExcept (format e ops) `shouldBe` Right "1-1"

  describe ("Expr6: " ++ eshow expr6) $ do
    let (e, ops) = expr6
    it "evaluated correctly" $ runExcept (eval e ops) `shouldBe` Right (-2.0)
    it "formated correctly" $ runExcept (format e ops) `shouldBe` Right "-1-1"

  describe ("Expr7: " ++ eshow expr7) $ do
    let (e, ops) = expr7
    it "evaluated correctly" $ runExcept (eval e ops) `shouldBe` Right (-10.0)
    it "formated correctly" $ runExcept (format e ops) `shouldBe` Right "5*(-2)"

  describe ("Expr8: " ++ eshow expr8) $ do
    let (e, ops) = expr8
    it "evaluated correctly" $ runExcept (eval e ops) `shouldBe` Right (-10.0)
    it "formated correctly" $ runExcept (format e ops) `shouldBe` Right "(-2)*5"

  describe ("Expr9: " ++ eshow expr9) $ do
    let (e, ops) = expr9
    it "evaluated correctly" $ runExcept (eval e ops) `shouldBe` Right 14.0
    it "formated correctly" $ runExcept (format e ops) `shouldBe` Right "2*(3+4)"

  describe ("Expr10: " ++ eshow expr10) $ do
    let (e, ops) = expr10
    it "evaluated correctly" $ runExcept (eval e ops) `shouldBe` Right 77.0
    it "formated correctly" $ runExcept (format e ops) `shouldBe` Right "(5+6)*7"

  describe ("Expr11: " ++ eshow expr11) $ do
    let (e, ops) = expr11
    it "evaluated correctly" $ runExcept (eval e ops) `shouldBe` Right (-18.0)
    it "formated correctly" $ runExcept (format e ops) `shouldBe` Right "(5+13)/(-2+1*(4-3))"

  describe ("Expr12: " ++ eshow expr12) $ do
    let (e, ops) = expr12
    it "evaluated correctly" $ runExcept (eval e ops) `shouldBe` Right 1.0
    it "formated correctly" $ runExcept (format e ops) `shouldBe` Right "1-1+1"

  describe ("Expr13: " ++ eshow expr12) $ do
    let (e, ops) = expr13
    it "corrupted" $ runExcept (eval e ops) `shouldBe` Left InvalidOperationsList
