module ExprGen where

import Expr
import System.Random

randomRMs :: (RandomGen g, Random a, Num a, Num n, Eq n) => a -> n -> g -> ([a], g)
randomRMs _ 0 gen = ([], gen)
randomRMs mod n gen = 
  let (a, newGen) = randomR (-mod, mod) gen
      (rest, lastGen) = randomRMs mod (n - 1) newGen 
   in (a:rest, lastGen) 

randomLiterals :: (RandomGen g) => Int -> Int -> g -> ([Int], g)
randomLiterals = randomRMs

randomExpr :: (RandomGen g, Num a) => [a] -> g -> (Expr a, g)
randomExpr [x] gen = (Val x, gen)
randomExpr xs gen =
  let (lh, gs) = randomR (1, length xs - 1) gen
      (l, r) = splitAt lh xs
      (le, gl) = randomExpr l gs
      (re, gr) = randomExpr r gl
   in (Expr le re, gr)

randomOps :: (RandomGen g) => Int -> g -> ([Op], g)
randomOps 0 gen = ([], gen)
randomOps n gen =
  let (op, newGen) = random gen
      (rest, lastGen) = randomOps (n - 1) newGen
   in (op:rest, lastGen)

succOpsUntil :: ([Op] -> Bool) -> [Op] -> [Op]
succOpsUntil check ops 
  | check ops = ops
  | otherwise = succOpsUntil check (succOps ops)

genExpr :: (RandomGen g) => Int -> Int -> g -> (String, g)
genExpr k m gen =
  let (ls, g1) = randomLiterals m k gen
      (e, g2) = randomExpr ls g1
      (ops, g3) = randomOps (k - 1) g2
      n = realToFrac m
      check ops = let res = eval e ops in res <=n && res >= -n
      goodOps = succOpsUntil (check) ops
   in (format e goodOps, g3) 
