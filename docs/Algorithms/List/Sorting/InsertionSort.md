# Insertion Sort

```hs
module Algorithms.List.Sorting.InsertionSort where

import Control.Arrow ((&&&))

import Data.Functor.Foldable
```

Insertion sort is an algorithm that inserts the elements of the list one by one into the sorted list. This can be written down in a straightforward manner using Catamorphism[^1].

```hs
-- | >>> insertionSortCata [1, 3, 2]
-- [1,2,3]
insertionSortCata :: Ord a => [a] -> [a]
insertionSortCata = cata \case
                      Nil       -> []
                      Cons x xs -> insert x xs
                    where
                    insert x xs = let (ys, zs) = span (<= x) xs
                                   in ys ++ [x] ++ zs
```

It can also be implemented using bialgebra and distribution laws, since both arguments and return values are lists. Surprisingly, given the duality of this implementation, we get a selection sort[^2].

```hs
swap :: Ord a => ListF a (ListF a r) -> ListF a (ListF a r)
swap Nil          = Nil
swap (Cons a Nil) = Cons a Nil
swap (Cons a (Cons b r))
  | a <= b    = Cons a (Cons b r)
  | otherwise = Cons b (Cons a r)

-- | >>> insertionSortCataAna [1, 3, 2]
-- [1,2,3]
insertionSortCataAna :: Ord a => [a] -> [a]
insertionSortCataAna = cata $ ana (swap . fmap project)
```

This implementation is a bit inefficient, so you can get a true insert sort by using Apomorphism. (In fact, the dual of this inefficient insertion sort will be the bubble sort.)

```hs
swop :: Ord a => ListF a (x, ListF a x) -> ListF a (Either x (ListF a x))
swop Nil               = Nil
swop (Cons a (x, Nil)) = Cons a (Left x)
swop (Cons a (x, (Cons b x')))
  | a <= b    = Cons a (Left x)
  | otherwise = Cons b (Right $ Cons a x')

-- | >>> insertionSortCataApo [1, 3, 2]
-- [1,2,3]
insertionSortCataApo :: Ord a => [a] -> [a]
insertionSortCataApo = cata $ apo (swop . fmap (id &&& project))
```

## References
[1] Augusteijn, Lex. "Sorting morphisms." International School on Advanced Functional Programming. Springer, Berlin, Heidelberg, 1998.  
[2] Hinze, Ralf, et al. "Sorting with bialgebras and distributive laws." Proceedings of the 8th ACM SIGPLAN workshop on Generic programming. 2012.
