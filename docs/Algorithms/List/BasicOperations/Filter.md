# filter

```hs
module Algorithms.List.BasicOperations.Filter where

import Data.Functor.Foldable
import GHC.Natural
```

This implementation can be found in Meijer (1991)[^1].

```hs
-- | >>> filterCata odd [1, 2, 3]
-- [1,3]
filterCata :: (a -> Bool) -> [a] -> [a]
filterCata p = cata \case
                 Nil        -> []
                 (Cons a b) -> if p a then a : b else b
```

## References
[1] Meijer, Erik, Maarten Fokkinga, and Ross Paterson. "Functional programming with bananas, lenses, envelopes and barbed wire." Conference on Functional Programming Languages and Computer Architecture. Springer, Berlin, Heidelberg, 1991.
