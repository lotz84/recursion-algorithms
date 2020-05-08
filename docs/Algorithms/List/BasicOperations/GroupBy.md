# groupBy


```hs
module Algorithm.List.BasicOperations.GroupBy where

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
