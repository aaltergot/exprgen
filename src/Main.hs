module Main where

import Expr

main :: IO ()
main = putStrLn "hello world"

exp1 = ef (Expr (Val 2) (Expr (Val 3) (Val 4))) [M, A]
exp2 = ef (Expr (Expr (Val 5) (Val 13)) (Expr (Val (-2)) (Expr (Val 1) (Expr (Val 4) (Val (-3)))))) [D, A, A, M, A]
exp3 = ef (Expr (Val 5) (Expr (Val 6) (Val 7))) [A, M]
exp4 = ef (Expr (Val 5) (Val (-2))) [M]
exp5 = ef (Expr (Val (-2)) (Val 5)) [M]




