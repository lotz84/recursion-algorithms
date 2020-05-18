# Run Length Conversion

```hs
module Algorithms.List.RunLengthConversion where

import Control.Arrow
import Data.List
import Data.Functor.Foldable
```

The function ``runLength``'s spec is following:

```hs
{- 
   @
   runLength :: Eq a => [a] -> [(a, Int)]
   runLength = map (head &&& length) . group
   @
-}
```

We can eliminate the intermediate list between ``map f`` and ``group`` by fusing them into an anamorphism.

```hs
{-
   @
      map f . ana psi
        where
          psi = \ case
            [] -> Nil
            xs -> Cons y ys
    = 
      ana psi'
        where
          psi = \ case
            [] -> Nil
            xs -> Cons (f y) ys
   @
-} 
```

We can fuse ``map (head &&& length) . group`` into an ana.

```hs
{-
   @
      map (head &&& length) . group
    = 
      map (head &&& length) . groupBy (==)
    = 
      map (head &&& length) . ana psi
        where
          psi = \ case
            []   -> Nil
            x:xs -> uncurry Cons (first (x:)) (span (x ==) xs))
    = 
      ana psi'
        where
          psi' = \ case
            []   -> Nil
            x:xs -> uncurry Cons (first ((head &&& length) . (x:))) (span (x ==) xs)
   @
-}
```

So far, we define ``runLength`` as an instance of anamorphism.

```hs
{-
   @
   runLength = ana psi
     where
       psi = \ case
         []   -> Nil
         x:xs -> uncurry Cons (first ((head &&& length) . (x:)) (span (x ==) xs))
   @
-}
```

If we have ``spanCount :: (a -> Bool) -> [a] -> (Int, [a])`` instead of ``span``, we can get a slightly more efficient definition: 

```hs
{- |
>>> xs = "mississippi"
>>> runLength xs == (map (head &&& length) . group) xs
True
-}

runLength :: Eq a => [a] -> [(a, Int)] 
runLength = ana psi
  where
    psi = \ case
      []   -> Nil
      x:xs -> uncurry Cons (first ((,) x . succ) (spanCount (x ==) xs))
```

The ``spanCount`` returns the length of the span instead of returning the span itself.  Its spec is:

```hs
{-
   @
   spanCount p = first length . span p 
   @
-}
```

We can define the ``spanCount`` as an instance of paramorphism.

```hs
{- |
>>> xs = [2,4,1,6,3,5]
>>> (spanCount even) xs == (first length . span even) xs
True
-}

spanCount :: (a -> Bool) -> [a] -> (Int, [a])
spanCount p = para phi
  where
    phi = \ case
      Nil            -> (0, [])
      Cons a (as, b) -> if p a then first succ b else (0, a:as)
```
