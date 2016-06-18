{-# LANGUAGE OverloadedStrings #-}

module Expr 
  ( Op (A, M, D)
  , succOps
  , Expr (Expr, Val)
  , eval
  , format
  , ef
  ) where

import           System.Random
import           Data.Text (Text, pack, append)

data Op = A -- Add
        | M -- Multiply
        | D -- Divide
        deriving (Eq, Enum, Bounded)

instance Show Op where
  show A = "+"
  show M = "*"
  show D = "/"

instance Random Op where
  randomR (a, b) gen = 
    let (i, newGen) = randomR (fromEnum a, fromEnum b) gen 
     in (toEnum i, newGen)
  random gen = randomR (minBound :: Op, maxBound :: Op) gen 

(+++) :: Text -> Text -> Text
(+++) = append

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

data Expr a = Val a 
            | Expr (Expr a) (Expr a)

instance (Show a) => Show (Expr a) where
  show (Val a) = show a
  show (Expr l r) = "(" ++ show l ++ " " ++ show r ++ ")"

nodeCnt :: Expr a -> Int
nodeCnt (Expr l r)  = 1 + nodeCnt l + nodeCnt r
nodeCnt _ = 0

eval :: (Real a, Real b, Fractional b) => Expr a -> [Op] -> b
eval (Val x) _ = realToFrac x
eval (Expr l r) (op:ops) = 
  let (lops, rops) = splitAt (nodeCnt l) ops
   in opFun op (eval l lops) (eval r rops)

format :: (Show a, Real a, Ord a) => Expr a -> [Op] -> Text
format (Val x) _ = pack $ show x
format e ops = formatInner A e ops

formatInner :: (Show a, Real a) => Op -> Expr a -> [Op] -> Text
formatInner _ (Val x) _ = 
  let s = pack $ show x 
   in if x < 0 then "(" +++ s +++ ")" else s
formatInner pop (Expr l r) (op:ops) = 
  let (lops, rops) = splitAt (nodeCnt l) ops
      needP = (opPrec pop) < (opPrec op) 
   in (if needP then "(" else "") +++ 
     (case l of 
       Val lv | lv < 0 && op == A -> pack $ show lv
       _ -> formatInner op l lops
     ) +++ 
       (case r of
          Val rv | rv < 0 && op == A -> pack $ show rv
          _ -> (pack $ show op) +++ (formatInner op r rops)
       ) +++ (if needP then ")" else "")

ef :: (Show a, Real a, Real b, Fractional b) => Expr a -> [Op] -> (b, Text)
ef e ops = (eval e ops, format e ops)
