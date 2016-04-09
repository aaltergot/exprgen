module Expr 
  ( Op (A, M, D)
  , succOp
  , succOps
  , Expr (Expr, Val)
  , eval
  , format
  , ef
  ) where

data Op = A -- Add
        | M -- Multiply
        | D -- Divide
        deriving (Eq)

instance Show Op where
  show A = "+"
  show M = "*"
  show D = "/"

succOp :: Op -> Op
succOp A = M
succOp M = D
succOp _ = A

succOps :: [Op] -> [Op]
succOps [] = []
succOps (op:ops) = let next = succOp op
                    in next:(if next == A then succOps ops else ops)

opFun :: (Real a, Fractional a) => Op -> a -> a -> a
opFun A = (+)
opFun M = (*)
opFun D = (/)

opPrec :: Op -> Int
opPrec A = 4
opPrec M = 3
opPrec D = 3

data Expr a = Val a 
            | Expr (Expr a) (Expr a) deriving (Show)

nodeCnt :: Expr a -> Int
nodeCnt (Expr l r)  = 1 + nodeCnt l + nodeCnt r
nodeCnt _ = 0

eval :: (Real a, Real b, Fractional b) => Expr a -> [Op] -> b
eval (Val x) _ = realToFrac x
eval (Expr l r) (op:ops) = let (lops, rops) = splitAt (nodeCnt l) ops
                            in opFun op (eval l lops) (eval r rops)

format :: (Show a, Real a, Ord a) => Expr a -> [Op] -> String
format (Val x) _ = 
  let s = show x 
   in if x < 0 then "(" ++ s ++ ")" else s
format (Expr l r) (op:ops) = 
  let (lops, rops) = splitAt (nodeCnt l) ops
   in formatInner op l lops ++ show op ++ formatInner op r rops

formatInner :: (Show a, Real a, Ord a) => Op -> Expr a -> [Op] -> String
formatInner _ (Val x) _ = 
  let s = show x 
   in if x < 0 then "(" ++ s ++ ")" else s
formatInner pop (Expr l r) (op:ops) = 
  let (lops, rops) = splitAt (nodeCnt l) ops
      needP = (opPrec pop) < (opPrec op) 
   in (if needP then "(" else "") ++ 
     (case l of 
       Val lv | lv < 0 && op == A -> show lv
       _ -> formatInner op l lops
     ) ++ 
       (case r of
          Val rv | rv < 0 && op == A -> show rv
          _ -> show op ++ formatInner op r rops
       ) ++ (if needP then ")" else "")

ef :: (Show a, Real a, Real b, Fractional b) => Expr a -> [Op] -> (b, String)
ef e ops = (eval e ops, format e ops)
