# Edit Distance

```hs
module Algorithms.List.EditDistance where

import GHC.Natural

import Control.Comonad
import Control.Comonad.Cofree
import Data.Functor.Foldable

import RecursionSchemes.Extra
```

Edit distance is an algorithm that calculates the distance between two strings, based on the idea that there is a cost every time you insert, delete, or replace a string when converting from one to the other. Check out [Edit distance - Wikipedia](https://en.wikipedia.org/wiki/Edit_distance) for more information.The implementation here is based on Kabanov (2006)[^1].

The editing distance algorithm looks like it could be implemented straightforwardly in Hylomorphism, but this is inefficient because it doesn't use the results once computed, resulting in duplicate computations.

```hs
-- | Base Functor for the intermediate structure of editDistHylo
data G c x = Inl [c] | Inr (c, c) (x, x, x)
instance Functor (G c) where
  fmap f (Inl cs) = Inl cs
  fmap f (Inr cc (a, b, c)) = Inr cc (f a, f b, f c)

-- | >>> editDistHylo ("kitten", "sitting")
-- 3
editDistHylo :: Eq a => ([a], [a]) -> Natural
editDistHylo = hylo g f
               where
               f ([], bs)     = Inl bs
               f (as, [])     = Inl as
               f (a:as, b:bs) = Inr (a, b) ((as, b:bs), (a:as, bs), (as, bs))
               g (Inl as) = fromIntegral $ length as
               g (Inr (a, b) (x1, x2, x3)) =
                 minimum [x1 + 1, x2 + 1, x3 + if a == b then 0 else 1]
```

It is possible to implement using Dynamorphism to use dynamic programming to use the results once they are computed. Edit distance algorithms usually consider tabular data as an intermediate structure, but matrix is not a recursive data type, so they do not fit well with recursion schemes. So, as an intermediate structure, we consider a list of a matrix decomposed into row by row.

```hs
-- | Base Functor for the intermediate structure of editDistDyna
data F c x = F [c] [c] (Maybe x)

instance Functor (F c) where
  fmap f (F as bs x) = F as bs (fmap f x)

-- | >>> editDistDyna ("kitten", "sitting")
-- 3
editDistDyna :: Eq a => ([a], [a]) -> Natural
editDistDyna (cs, cs') =
  let n = length cs
   in dyna (g n) f (cs, cs')
  where
  f ([] , [])  = F []     []     Nothing
  f ([], b:bs) = F []     (b:bs) (Just (cs, bs))
  f (a:as, bs) = F (a:as) bs     (Just (as, bs))
  g _ (F _      _      Nothing)  = 0
  g _ (F []     bs     (Just _)) = fromIntegral $ length bs
  g _ (F as     []     (Just _)) = fromIntegral $ length as
  g n (F (a:as) (b:bs) (Just x)) =
    minimum [ extract x + 1
            , extract (iterate pi x !! n) + 1
            , extract (iterate pi x !! (n + 1)) + if a == b then 0 else 1
            ]
  pi (_ :< F _ _ (Just y)) = y
```

## References
[1] Kabanov, Jevgeni, and Varmo Vene. "Recursion schemes for dynamic programming." International Conference on Mathematics of Program Construction. Springer, Berlin, Heidelberg, 2006.
