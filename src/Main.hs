module Main where

main :: IO ()
main = do
  putStrLn "hello world"


data Op = A -- Add
        | M -- Multiply
        | D -- Divide
        deriving (Show, Eq)

succOp :: Op -> Op
succOp A = M
succOp M = D
succOp _ = A

succOps :: [Op] -> [Op]
succOps [] = []
succOps (op:ops) = let next = succOp op
                    in next:(if next == A then succOps ops else ops)

data Expr a = Val a 
            | Expr (Expr a) (Expr a) deriving (Show)

nodeCnt :: Expr a -> Int
nodeCnt (Expr l r)  = 1 + nodeCnt l + nodeCnt r
nodeCnt _ = 0
