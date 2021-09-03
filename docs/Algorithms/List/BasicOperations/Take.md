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

It can be implemented using catamorphism through the adjoint fold about the adjunction of -×A -\| -^A [^2].

```hs
-- | >>> takeCata 3 [1..10]
-- [1,2,3]
takeCata :: Natural -> [a] -> [a]
takeCata = cata f
  where
  f Nothing  _      = []
  f _        []     = []
  f (Just g) (x:xs) = x : g xs
```

## drop

`drop` can also be implemented using catamorphism through the adjoint fold[^2].

```hs
-- | >>> dropCata 3 [1..5]
-- [4,5]
dropCata :: Natural -> [a] -> [a]
dropCata = cata f
  where
  f Nothing  xs     = xs
  f _        []     = []
  f (Just g) (x:xs) = g xs
```

`drop` can be implemented using Monoidal Catamorphism[^3].

```hs
-- | >>> dropCat 3 [1..5]
-- [4,5]
dropCat :: Natural -> [a] -> [a]
dropCat n = cat listFold (L.drop n L.list) . refix
```

## References
[1] Uustalu, Tarmo, and Varmo Vene. "Coding recursion a la Mendler." Department of Computer Science, Utrecht University. 2000.  
[2] Hinze, Ralf. "Adjoint folds and unfolds." International Conference on Mathematics of Program Construction. Springer, Berlin, Heidelberg, 2010.  
[3] [Monoidal Catamorphisms \| Bartosz Milewski's Programming Cafe](https://bartoszmilewski.com/2020/06/15/monoidal-catamorphisms/)  
