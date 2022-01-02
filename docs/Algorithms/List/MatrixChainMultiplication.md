# Matrix Chain Multiplication

```hs
module Algorithms.List.MatrixChainMultiplication where

import Control.Comonad.Cofree

import RecursionSchemes.Extra
```

Matrix Chain Multiplication is an algorithm for finding the order of computation that minimizes the amount of computation for computing the matrix product of given matrices. Check out [Matrix chain multiplication - Wikipedia](https://en.wikipedia.org/wiki/Matrix_chain_multiplication) for more information on the algorithm.

```hs
-- | Base Functor for the intermediate structure
data F x = F Int Int (Maybe x) deriving Functor

-- | >>> mcm [100, 500, 1000, 5000, 10000]
-- 5550000000
mcm :: [Int] -> Int
mcm xs = dyna g f (0, n-1)
  where
  n = length xs - 1
  f (i, j)
    | i == 0 && j == 0 = F i j Nothing
    | i == 0           = F i j (Just (n-j, n-1))
    | otherwise        = F i j (Just (i-1, j-1))
  g (F _ _ Nothing) = 0
  g (F i j (Just cs))
    | i == j = 0
    | otherwise = minimum $ flip map [i..j-1] $ \k ->
        let posR = pos i j - pos i k     - 1
            posL = pos i j - pos (k+1) j - 1
            cost = xs!!i * xs !!(k+1) * xs!!(j+1)
         in lookup cs posR + lookup cs posL + cost
  pos i j = n * (j-i) - (div ((j-i)*(j-i-1)) 2) + i
  lookup (a :< _)               0 = a
  lookup (_ :< F _ _ (Just cs)) n = lookup cs (n-1)
```
