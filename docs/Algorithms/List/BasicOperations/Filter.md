# filter

```hs
module Algorithms.List.BasicOperations.Filter where

import Data.Functor.Foldable
import GHC.Natural

import RecursionSchemes.Extra
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

You can also implement a monadic filter, which is used to implement subsequences[^2].

```hs
-- | >>> filterCataM (pure . odd) [1, 2, 3]
-- [1,3]
filterCataM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterCataM p = cataM \case
                  Nil       -> pure []
                  Cons x xs -> do
                    flg <- p x
                    pure if flg then x:xs else xs
```

## References
[1] Meijer, Erik, Maarten Fokkinga, and Ross Paterson. "Functional programming with bananas, lenses, envelopes and barbed wire." Conference on Functional Programming Languages and Computer Architecture. Springer, Berlin, Heidelberg, 1991.  
[2] [Monadic versions · Issue #5 · vmchale/recursion_schemes](https://github.com/vmchale/recursion_schemes/issues/5)