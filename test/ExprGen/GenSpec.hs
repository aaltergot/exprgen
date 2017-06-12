{-# LANGUAGE OverloadedStrings #-}

module ExprGen.GenSpec where

import Control.Monad.Except (runExcept)
import Control.Monad.Random (evalRandT)
import System.Random
import Test.Hspec

import ExprGen.Gen

spec :: Spec
spec =
  describe "genExpr 10 20 (mkStdGen 1)" $ do
    let res = runExcept (evalRandT (generateExpr $ Config 10 20) (mkStdGen 1))
    it "generated" $ (formatted <$> res) `shouldBe` Right "(-4-4+3/(-4))/18*(-15)/(18*17/3-14)"
