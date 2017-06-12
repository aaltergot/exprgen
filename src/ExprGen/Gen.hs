{-# LANGUAGE FlexibleContexts #-}
module ExprGen.Gen
  ( Config(..)
  , Result(..)
  , generateExpr
  ) where

import Control.Monad.Except (MonadError)
import Control.Monad.Random (Random, MonadRandom, getRandomR, getRandom)
import Data.Text (Text)

import ExprGen.Expr

data Config = Config
  { literalsNum :: Int
  , valueMod :: Int
  } deriving (Show)

data Result = Result
  { expression :: Expr Int
  , operations :: [Op]
  , formatted :: Text
  , value :: Double
  } deriving (Show)

randomRMs :: (MonadRandom r, Random m, Num m) => Int -> m -> r [m]
randomRMs 0 _ = return []
randomRMs n m = do
  v <- getRandomR (-m, m)
  rest <- randomRMs (n - 1) m
  return $ v:rest

randomExpr :: (MonadRandom r, Num a) => [a] -> r (Expr a)
randomExpr [x] = return $ Val x
randomExpr xs = do
  leftHeight <- getRandomR (1, length xs - 1)
  let (l, r) = splitAt leftHeight xs
  le <- randomExpr l
  re <- randomExpr r
  return $ Expr le re

randomOps :: (MonadRandom r) => Int -> r [Op]
randomOps 0 = return []
randomOps n = do
  op <- getRandom
  rest <- randomOps (n -1)
  return $ op:rest


generateExpr :: (MonadRandom m, MonadError ExprException m)
             => Config -> m Result
generateExpr cfg = do
  literals <- randomLiterals
  expr <- randomExpr literals
  ops0 <- randomOps $ literalsNum cfg - 1
  ops <- succOpsUntil ops0 (checkValueModule (realToFrac $ valueMod cfg) expr)
  fmt <- format expr ops
  v <- eval expr ops
  return $ Result expr ops fmt v
  where

    randomLiterals :: (MonadRandom m) => m [Int]
    randomLiterals = randomRMs (literalsNum cfg) (valueMod cfg)

    checkValueModule :: (MonadError ExprException m) => Double -> Expr Int -> [Op] -> m Bool
    checkValueModule m e o = do
      r <- eval e o
      return (r <= m && r >= -m)

    succOpsUntil :: (MonadError ExprException m) => [Op] -> ([Op] -> m Bool) -> m [Op]
    succOpsUntil ops check = do
      done <- check ops
      if done then return ops else succOpsUntil (succOps ops) check
