# length

```hs
module Algorithms.List.BasicOperations.Length where

import Data.Monoid
import GHC.Natural

import qualified Control.Foldl as L
import Data.Functor.Foldable

import DataStructures.List
import RecursionSchemes.Extra
```

Since the length function is a list to natural number function, it can be represented as either a cata or ana. This is the most basic implementation by cata and was introduced in Meijer(1991)[^1].

```hs
-- | >>> lengthCata [1, 2, 3]
-- 3
lengthCata :: [a] -> Natural
lengthCata = cata \case
               Nil      -> 0
               Cons _ n -> 1 + n
```

Conversely, if we look at the natural number of return values, we get an implementation by ana.

```hs
-- | >>> lengthAna [1, 2, 3]
-- 3
lengthAna :: [a] -> Natural
lengthAna = ana \case
              []     -> Nothing
              (_:xs) -> Just xs
```

Given that both input and output are recursive types, it is possible that the algorithm could be written down using bialgebra and distributional laws[^2]. In this case this is correct. ListF is for lists and Maybe is for Natural's Base Functor.

```hs
dist :: ListF a (Maybe b) -> Maybe (ListF a b)
dist Nil               = Nothing
dist (Cons a Nothing)  = Just Nil
dist (Cons a (Just b)) = Just (Cons a b)


-- | >>> lengthCataAna [1, 2, 3]
-- 3
lengthCataAna :: [a] -> Natural
lengthCataAna = cata $ ana (dist . fmap project)


-- | >>> lengthAnaCata [1, 2, 3]
-- 3
lengthAnaCata :: [a] -> Natural
lengthAnaCata = ana $ cata (fmap embed . dist)
```

`length` can also be implemented using Monoidal Catamorphism[^3].

```hs
-- | >>> lengthCat [1, 2, 3]
-- 3
lengthCat :: [a] -> Natural
lengthCat = cat listFold L.genericLength . refix
```

## References
[1] Meijer, Erik, Maarten Fokkinga, and Ross Paterson. "Functional programming with bananas, lenses, envelopes and barbed wire." Conference on Functional Programming Languages and Computer Architecture. Springer, Berlin, Heidelberg, 1991.  
[2] Hinze, Ralf, et al. "Sorting with bialgebras and distributive laws." Proceedings of the 8th ACM SIGPLAN workshop on Generic programming. 2012.  
[3] [Monoidal Catamorphisms \| Bartosz Milewski's Programming Cafe](https://bartoszmilewski.com/2020/06/15/monoidal-catamorphisms/)  
