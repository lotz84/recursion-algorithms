# groupBy


```hs
module Algorithms.List.BasicOperations.GroupBy where

import Control.Arrow (first)
import Data.Functor.Foldable
```

``groupBy`` is defined in standard Data.List module of base package. Its spec is following

```hs
{- @
   groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
   groupBy _  []     = []
   groupBy eq (x:xs) = (x:ys) : groupBy eq zs
                       where (ys,zs) = span (eq x) xs
   @
-}
```

We can implement ``groupBy`` as an instance of paramorphism.

```hs
{- | >>> groupByAna (==) "Mississippi"
["M","i","ss","i","ss","i","pp","i"]
-}
groupByAna :: (a -> a -> Bool) -> [a] -> [[a]]
groupByAna eq = ana psi
  where
    psi = \ case
      []   -> Nil
      x:xs -> uncurry Cons (first (x :) (span (eq x) xs))
```
And we can also implement ``groupBy`` as an instance of catamorphism. [*](https://github.com/lotz84/recursion-algorithms/pull/2#issuecomment-626012637)

```hs
{- | >>> groupByCata (==) "Mississippi"
["M","i","ss","i","ss","i","pp","i"]
-}
groupByCata :: (a -> a -> Bool) -> [a] -> [[a]]
groupByCata g = cata alg
  where
    alg = \ case
      Nil           -> []
      Cons x []     -> [[x]]
      Cons x (ys@(y:_):yss)
        | g x y     -> (x:ys):yss
        | otherwise -> [x]:ys:yss
```
