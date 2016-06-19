module ExprGen 
  ( ExprGenConfig(..)
  , ExprGenResult(..)
  , genExpr
  ) where

import           Expr
import           System.Random
import           Control.Monad.State
import           Data.Text (Text)

data ExprGenConfig = ExprGenConfig { getN :: Int
                                   , getM :: Int } deriving (Show)

data ExprGenResult a b = ExprGenResult { getExpr :: Expr a
                                       , getExprAsText :: Text
                                       , getOps :: [Op]
                                       , getValue :: b
                                       } deriving (Show)

randomRMs :: (RandomGen g, Random m, Num m, Num n, Eq n) => n -> m -> State g [m]
randomRMs 0 _ = return []
randomRMs n mod = do
  m <- state $ randomR (-mod, mod)
  rest <- randomRMs (n - 1) mod 
  return $ m:rest

randomLiterals :: (RandomGen g) => ExprGenConfig -> State g [Int]
randomLiterals config = randomRMs (getN config) (getM config)

randomExpr :: (RandomGen g, Num a) => [a] -> State g (Expr a)
randomExpr [x] = return $ Val x
randomExpr xs = do
  leftHeight <- state $ randomR (1, length xs - 1)
  let (l, r) = splitAt leftHeight xs
  le <- randomExpr l
  re <- randomExpr r
  return $ Expr le re

randomOps :: (RandomGen g) => Int -> State g [Op]
randomOps 0 = return []
randomOps n = do
  op <- state random
  rest <- randomOps (n -1)
  return $ op:rest

succOpsUntil :: ([Op] -> Bool) -> [Op] -> [Op]
succOpsUntil check ops 
  | check ops = ops
  | otherwise = succOpsUntil check (succOps ops)

genExpr :: (RandomGen g, Real b, Fractional b) 
        => ExprGenConfig -> State g (ExprGenResult Int b)
genExpr config = do
  literals <- randomLiterals config
  expr <- randomExpr literals
  ops <- randomOps $ getN config - 1
  let m = realToFrac $ getM config
      check ops = let res = eval expr ops in res <= m && res >= -m
      goodOps = succOpsUntil check ops
  return $ ExprGenResult expr (format expr goodOps) goodOps (eval expr goodOps)
