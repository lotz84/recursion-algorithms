# Longest Common Subsequence

```hs
module Algorithms.List.LongestCommonSubsequence where

import GHC.Natural

import Control.Comonad
import Control.Comonad.Cofree
import Data.Functor.Foldable

import RecursionSchemes.Extra
```

Longest Common Subsequence is an algorithm that extracts the longest common sequence from a given two lists. Check out [Longest common subsequence problem - Wikipedia](https://en.wikipedia.org/wiki/Longest_common_subsequence_problem) for more information on the algorithm. The algorithm looks like it could be written down in a straightforward manner using Hylomorphism. However, this implementation is inefficient because it results in a number of duplicate calculations.

```hs
-- | Base Functor for the intermediate structure of lcsHylo
data G c x = G (Maybe (c, c, x, x, x))

instance Functor (G c) where
  fmap f (G Nothing) = G Nothing
  fmap f (G (Just (c1, c2, x1, x2, x3))) = G (Just (c1, c2, f x1, f x2, f x3))

-- | >>> lcsHylo ("kitten", "sitting")
-- "ittn"
lcsHylo :: Eq a => ([a], [a]) -> [a]
lcsHylo = hylo g f
          where
          f ([], _) = G Nothing
          f (_, []) = G Nothing
          f (a:as, b:bs) = G $ Just (a, b, (as, b:bs), (a:as, bs), (as, bs))
          g (G Nothing) = []
          g (G (Just (a, b, x1, x2, x3))) =
            if a == b then a:x3 else (if length x1 > length x2 then x1 else x2)
```

Dynamorphism can be used to rewrite this implementation into an algorithm that does not do any extra computation[^1].

```hs
-- | Base Functor for the intermediate structure of lcsDyna
data F c x = F [c] [c] (Maybe x)

instance Functor (F c) where
  fmap f (F as bs x) = F as bs (fmap f x)

-- | >>> lcsDyna ("kitten", "sitting")
-- "ittn"
lcsDyna :: Eq a => ([a], [a]) -> [a]
lcsDyna (cs, cs') =
  let n = length cs
   in dyna (g n) f (cs, cs')
  where
  f ([],   [])   = F []     []     Nothing
  f ([],   b:bs) = F []     (b:bs) (Just (cs, bs))
  f (a:as, bs)   = F (a:as) bs     (Just (as, bs))
  g _ (F _      _      Nothing)  = []
  g _ (F []     _      _)        = []
  g _ (F _      []     _)        = []
  g n (F (a:as) (b:bs) (Just x)) =
    let x1 = extract x
        x2 = extract $ iterate pi x !! n
        x3 = extract $ iterate pi x !! (n + 1)
     in if a == b then a:x3 else (if length x1 > length x2 then x1 else x2)
  pi (_ :< F _ _ (Just y)) = y
```

## References
[1] Kabanov, Jevgeni, and Varmo Vene. "Recursion schemes for dynamic programming." International Conference on Mathematics of Program Construction. Springer, Berlin, Heidelberg, 2006.
