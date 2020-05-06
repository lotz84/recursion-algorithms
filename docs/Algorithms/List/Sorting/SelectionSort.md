# Selection Sort

```hs
module Algorithms.List.Sorting.SelectionSort where

import Control.Arrow ((|||))

import Data.Functor.Foldable
```

Selection sort is an algorithm that sorts a list by repeating the operation to get the minimum value from the list. This can be implemented using Anamorphism[^1].

```hs
-- | >>> selectionSortAna [1, 3, 2]
-- [1,2,3]
selectionSortAna :: (Eq a, Ord a) => [a] -> [a]
selectionSortAna = ana \case
                     [] -> Nil
                     xs -> let x   = minimum xs
                               xs' = filter (/= x) xs
                            in Cons x xs'
```

This can be implemented as a dual of insertion sort using Anamorphism and Paramorphism[^2].

```hs
swop :: Ord a => ListF a (x, ListF a x) -> ListF a (Either x (ListF a x))
swop Nil               = Nil
swop (Cons a (x, Nil)) = Cons a (Left x)
swop (Cons a (x, (Cons b x')))
  | a <= b    = Cons a (Left x)
  | otherwise = Cons b (Right $ Cons a x')

-- | >>> selectionSortAnaPara [1, 3, 2]
-- [1,2,3]
selectionSortAnaPara :: Ord a => [a] -> [a]
selectionSortAnaPara = ana $ para (fmap (id ||| embed) . swop)
```

## References
[1] Augusteijn, Lex. "Sorting morphisms." International School on Advanced Functional Programming. Springer, Berlin, Heidelberg, 1998.  
[2] Hinze, Ralf, et al. "Sorting with bialgebras and distributive laws." Proceedings of the 8th ACM SIGPLAN workshop on Generic programming. 2012.
