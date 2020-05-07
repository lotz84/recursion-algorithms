# dropWhile

```hs
module Algorithms.List.BasicOperations.DropWhile where

import Data.Functor.Foldable

import RecursionSchemes.Extra
```

You can implement `dropWhile` by using paramorphism and returning the list of the points where the element no longer satisfies the condition.

```hs
-- | >>> dropWhilePara odd [3, 1, 4, 1, 5]
-- [4,1,5]
dropWhilePara :: (a -> Bool) -> [a] -> [a]
dropWhilePara p = para \case
    Nil             -> []
    Cons x (xs, ys) -> if p x then ys else x:xs
```

Monadic dropWhile also uses paramorphism, but the difference is that the object to be folded is wrapped in the monad[^1].

```hs
-- | >>> dropWhileParaM (\i -> print i >> pure (odd i)) [3, 1, 4, 1, 5]
-- 3
-- 1
-- 4
-- [4,1,5]
dropWhileParaM :: Monad m => (a -> m Bool) -> [a] -> m [a]
dropWhileParaM p = para \case
    Nil             -> pure []
    Cons x (xs, ys) -> do
      flg <- p x
      if flg then ys else pure (x:xs)
```

## References
[1] [Monadic versions · Issue #5 · vmchale/recursion_schemes](https://github.com/vmchale/recursion_schemes/issues/5)  