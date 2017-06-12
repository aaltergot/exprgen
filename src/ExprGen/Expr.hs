{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module ExprGen.Expr
  ( ExprException(..)
  , Op (A, M, D)
  , succOps
  , Expr (Expr, Val)
  , eval
  , format
  ) where

import Control.Monad.Except (MonadError(throwError))
import Data.Monoid ((<>))
import Data.Text (Text, isPrefixOf, pack)
import System.Random (Random, random, randomR)

data ExprException = InvalidOperationsList
  deriving (Show, Eq)

-- |Operations:
--  A - Add
--  M - Multiply
--  D - Divide
data Op = A | M | D deriving (Eq, Enum, Bounded)

instance Show Op where
  show A = "+"
  show M = "*"
  show D = "/"

instance Random Op where
  randomR (a, b) gen =
    let (i, newGen) = randomR (fromEnum a, fromEnum b) gen
     in (toEnum i, newGen)
  random = randomR (minBound :: Op, maxBound :: Op)

succOps :: [Op] -> [Op]
succOps [] = []
succOps (op:ops)
  | op == maxBound = minBound : succOps ops
  | otherwise = succ op : ops

opFun :: (Real a, Fractional a) => Op -> a -> a -> a
opFun A = (+)
opFun M = (*)
opFun D = (/)

opPrec :: Op -> Int
opPrec A = 4
opPrec M = 3
opPrec D = 3

data Expr a = Val a | Expr (Expr a) (Expr a)

instance (Show a) => Show (Expr a) where
  show (Val a) = show a
  show (Expr l r) = "(" <> show l <> " " <> show r <> ")"

nodeCnt :: Expr a -> Int
nodeCnt (Expr l r)  = 1 + nodeCnt l + nodeCnt r
nodeCnt _ = 0

asText :: Show a => a -> Text
asText = pack . show

eval :: (Real a, MonadError ExprException m)
     => Expr a -> [Op] -> m Double
eval (Val x) _ = return $ realToFrac x
eval (Expr l r) (op:ops) = do
  let (lops, rops) = splitAt (nodeCnt l) ops
  lv <- eval l lops
  rv <- eval r rops
  return $ opFun op lv rv
eval (Expr _ _) [] = throwError InvalidOperationsList

format :: (Show a, Real a, Ord a, MonadError ExprException m)
       => Expr a -> [Op] -> m Text
format (Val x) [] = return $ asText x
format (Val _) (_:_) = throwError InvalidOperationsList
format e ops = formatInner A e ops

formatInner :: (Show a, Real a, MonadError ExprException m)
            => Op -> Expr a -> [Op] -> m Text
formatInner _ (Val x) [] = do
  let s = asText x
  return $ if x < 0 then "(" <> s <> ")" else s
formatInner _ (Val _) (_:_) = throwError InvalidOperationsList
formatInner pop (Expr l r) (op:ops) = do
  let (lops, rops) = splitAt (nodeCnt l) ops
      needP = opPrec pop < opPrec op
  left <- case l of
    Val lv | lv < 0 && op == A -> return $ asText lv
    _ -> formatInner op l lops
  right <- case r of
    Val rv | rv < 0 && op == A -> return $ asText rv
    _ -> do
      re <- formatInner op r rops
      if "-" `isPrefixOf` re then return re else return $ asText op <> re
  return $ (if needP then "(" else "") <> left <> right <> (if needP then ")" else "")
formatInner _ (Expr _ _) [] = throwError InvalidOperationsList
