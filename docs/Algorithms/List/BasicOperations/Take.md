# take / drop

```hs
module Algorithms.List.BasicOperations.Take where

import GHC.Natural

import qualified Control.Foldl as L
import Data.Functor.Foldable

import DataStructures.List
import RecursionSchemes.Extra
```

## take

This implementation can be found in Uustalu (2000)[^1].

```hs
-- | >>> takeMMulti 3 [1..]
-- [1,2,3]
takeMMulti :: Natural -> [a] -> [a]
takeMMulti = mmulti phi
  where
  phi _ Nothing Nil         = []
  phi _ Nothing (Cons _ _)  = []
  phi _ (Just _) Nil        = []
  phi f (Just a) (Cons b c) = b : f a c
```

## drop

`drop` can be implemented using Monoidal Catamorphism[^2].

```hs
-- | >>> dropCat 3 [1..5]
-- [4,5]
dropCat :: Natural -> [a] -> [a]
dropCat n = cat listFold (L.drop n L.list) . refix
```

## References
[1] Uustalu, Tarmo, and Varmo Vene. "Coding recursion a la Mendler." Department of Computer Science, Utrecht University. 2000.  
[2] [Monoidal Catamorphisms \| Bartosz Milewski's Programming Cafe](https://bartoszmilewski.com/2020/06/15/monoidal-catamorphisms/)  
