# reverse

```hs
module Algorithms.List.BasicOperations.Reverse where

import Data.Functor.Foldable
```

This implementation by cata can be found in Meijer (1991)[^1].

```hs
-- | >>> reverseCata [1, 2, 3]
-- [3,2,1]
reverseCata :: [a] -> [a]
reverseCata = cata \case
                Nil        -> []
                (Cons a b) -> b ++ [a]
```

At first glance, implementation by ana looks difficult, but it's possible to do it via the difference list. This is an implementation in hylo, since it is expanded in ana to a list of difference lists and then folded in cata.

```hs
-- | >>> reverseHylo [1, 2, 3]
-- [3,2,1]
reverseHylo :: [a] -> [a]
reverseHylo = hylo f g
              where
              f Nil        = []
              f (Cons a b) = a b
              g []     = Nil
              g (x:xs) = Cons (++ [x]) xs
```

Given that reverse is a list to list function, it is natural to try to see if it can be implemented with bialgebra and the distribution law[^2]. And it was a success.

```hs
dist :: ListF a (ListF a b) -> ListF a (ListF a b)
dist Nil                 = Nil
dist (Cons a Nil)        = Cons a Nil
dist (Cons a (Cons b c)) = Cons b (Cons a c)


-- | >>> reverseCataAna [1, 2, 3]
-- [3,2,1]
reverseCataAna :: [a] -> [a]
reverseCataAna = cata $ ana (dist . fmap project)


-- | >>> reverseAnaCata [1, 2, 3]
-- [3,2,1]
reverseAnaCata :: [a] -> [a]
reverseAnaCata = ana $ cata (fmap embed . dist)
```

## References
[1] Meijer, Erik, Maarten Fokkinga, and Ross Paterson. "Functional programming with bananas, lenses, envelopes and barbed wire." Conference on Functional Programming Languages and Computer Architecture. Springer, Berlin, Heidelberg, 1991.  
[2] Hinze, Ralf, et al. "Sorting with bialgebras and distributive laws." Proceedings of the 8th ACM SIGPLAN workshop on Generic programming. 2012.
