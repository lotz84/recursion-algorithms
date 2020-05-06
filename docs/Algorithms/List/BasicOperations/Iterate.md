# iterate

```hs
module Algorithms.List.BasicOperations.Iterate where

import Data.Functor.Foldable
```

This implementation can be found in Meijer (1991)[^1].

```hs
-- | >>> take 3 $ iterateAna (+1) 1
-- [1,2,3]
iterateAna :: (a -> a) -> a -> [a]
iterateAna f = ana \a -> Cons a (f a)
```

## References
[1] Meijer, Erik, Maarten Fokkinga, and Ross Paterson. "Functional programming with bananas, lenses, envelopes and barbed wire." Conference on Functional Programming Languages and Computer Architecture. Springer, Berlin, Heidelberg, 1991.
