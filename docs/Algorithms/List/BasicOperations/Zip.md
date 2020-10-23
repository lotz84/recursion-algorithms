# zip

```hs
module Algorithms.List.BasicOperations.Zip where

import Data.Functor.Foldable

import RecursionSchemes.Extra
```

This implementation can be found in Meijer (1991)[^1].

```hs
-- | >>> zipAna ([1, 2, 3], "abc")
-- [(1,'a'),(2,'b'),(3,'c')]
zipAna :: ([a], [b]) -> [(a, b)]
zipAna = ana \case
           ([], _)      -> Nil
           (_, [])      -> Nil
           (a:as, b:bs) -> Cons (a, b) (as, bs)
```

Also zip can be implimented by using Multimorphism.

```hs
-- | >>> zipMMulti [1, 2, 3] "abc"
-- [(1,'a'),(2,'b'),(3,'c')]
zipMMulti :: [a] -> [b] -> [(a, b)]
zipMMulti = mmulti phi
  where
  phi _ Nil         Nil         = []
  phi _ Nil         (Cons _ _)  = []
  phi _ (Cons _ _)  Nil         = []
  phi f (Cons a as) (Cons b bs) = (a, b) : f as bs
```

## References
[1] Meijer, Erik, Maarten Fokkinga, and Ross Paterson. "Functional programming with bananas, lenses, envelopes and barbed wire." Conference on Functional Programming Languages and Computer Architecture. Springer, Berlin, Heidelberg, 1991.
