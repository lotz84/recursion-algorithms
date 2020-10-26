# map

```hs
module Algorithms.List.BasicOperations.Map where

import qualified Control.Foldl as L

import Data.Functor.Foldable

import DataStructures.List
import RecursionSchemes.Extra
```

The map can be easily implemented in both cata and ana to maintain the structure of the list. The implementation by ana is also described in Meijer (1991)[^1].

```hs
-- | >>> mapCata show [1,2,3]
-- ["1","2","3"]
mapCata :: (a -> b) -> [a] -> [b]
mapCata f = cata \case
              Nil        -> []
              (Cons a b) -> f a : b


-- | >>> mapAna show [1,2,3]
-- ["1","2","3"]
mapAna :: (a -> b) -> [a] -> [b]
mapAna f = ana \case
              []     -> Nil
              (x:xs) -> Cons (f x) xs
```

Since map is the endomorphism of the list, let's see if we can implement it with bialgebra and distributional laws. This generally works well, but the list is reversed at the end, so you'll need to fix it in reverse.

```hs
dist :: (a -> b) -> ListF a (ListF b c) -> ListF b (ListF a c)
dist _ Nil                 = Nil
dist f (Cons a Nil)        = Cons (f a) Nil
dist f (Cons a (Cons b c)) = Cons b (Cons a c)


-- | >>> mapCataAna show [1,2,3]
-- ["1","2","3"]
mapCataAna :: (a -> b) -> [a] -> [b]
mapCataAna f = reverse . (cata $ ana (dist f . fmap project))


-- | >>> mapAnaCata show [1,2,3]
-- ["1","2","3"]
mapAnaCata :: (a -> b) -> [a] -> [b]
mapAnaCata f = reverse . (ana $ cata (fmap embed . dist f))
```

`map` can also be implemented using Monoidal Catamorphism[^2].

```hs
-- | >>> mapCat show [1, 2, 3]
-- ["1","2","3"]
mapCat :: (a -> b) -> [a] -> [b]
mapCat f = cat listFold (L.premap f L.list) . refix
```

## References
[1] Meijer, Erik, Maarten Fokkinga, and Ross Paterson. "Functional programming with bananas, lenses, envelopes and barbed wire." Conference on Functional Programming Languages and Computer Architecture. Springer, Berlin, Heidelberg, 1991.  
[2] [Monoidal Catamorphisms \| Bartosz Milewski's Programming Cafe](https://bartoszmilewski.com/2020/06/15/monoidal-catamorphisms/)  
