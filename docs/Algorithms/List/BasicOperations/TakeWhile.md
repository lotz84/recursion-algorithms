# takeWhile

```hs
module Algorithms.List.BasicOperations.TakeWhile where

import Data.Functor.Foldable
```

This implementation can be found in Meijer (1991)[^1].

```hs
-- | >>> takeWhileCata even [2, 4, 7]
-- [2,4]
takeWhileCata :: (a -> Bool) -> [a] -> [a]
takeWhileCata p = cata \case
                    Nil      -> []
                    Cons a b -> if p a then a : b else []
```

## References
[1] Meijer, Erik, Maarten Fokkinga, and Ross Paterson. "Functional programming with bananas, lenses, envelopes and barbed wire." Conference on Functional Programming Languages and Computer Architecture. Springer, Berlin, Heidelberg, 1991.
