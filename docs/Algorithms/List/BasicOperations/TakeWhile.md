# takeWhile

```hs
module Algorithms.List.BasicOperations.TakeWhile where

import Data.Functor.Foldable
```

This implementation can be found in Meijer (1991)[^1].

```hs
-- | >>> takeWhileCata odd [3, 1, 4, 1, 5]
-- [3,1]
takeWhileCata :: (a -> Bool) -> [a] -> [a]
takeWhileCata p = cata \case
                    Nil      -> []
                    Cons a b -> if p a then a : b else []
```

Monadic takeWhile also uses catamorphism, but the difference is that the object to be folded is wrapped in the monad[^2].

```hs
-- | >>> takeWhileCataM (\i -> print i >> pure (odd i)) [3, 1, 4, 1, 5]
-- 3
-- 1
-- 4
-- [3,1]
takeWhileCataM :: Monad m => (a -> m Bool) -> [a] -> m [a]
takeWhileCataM p = cata \case
                     Nil       -> pure []
                     Cons x xs -> do
                       flg <- p x
                       if flg then (x:) <$> xs else pure []
```

## References
[1] Meijer, Erik, Maarten Fokkinga, and Ross Paterson. "Functional programming with bananas, lenses, envelopes and barbed wire." Conference on Functional Programming Languages and Computer Architecture. Springer, Berlin, Heidelberg, 1991.  
[2] [Monadic versions · Issue #5 · vmchale/recursion_schemes](https://github.com/vmchale/recursion_schemes/issues/5)