{-# LANGUAGE OverloadedStrings #-}

module ExprGenSpec where

import           ExprGen
import           System.Random
import           Test.Hspec
import           Control.Monad.State

spec :: Spec
spec = do

  describe "genExpr 10 20 (mkStdGen 1)" $ do 
    let (s, g) = runState (genExpr $ ExprGenConfig 10 20) (mkStdGen 1) 
     in it "generates" $ do 
      s `shouldBe` "(-4-4+3/(-4))/18*(-15)/(18*17/3-14)"
