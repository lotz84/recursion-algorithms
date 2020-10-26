# filter

```hs
module Algorithms.List.BasicOperations.Filter where

import GHC.Natural

import qualified Control.Foldl as L
import Data.Functor.Foldable

import DataStructures.List
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

`filter` can also be implemented using Monoidal Catamorphism[^3].

```hs
-- | >>> filterCat odd [1, 2, 3]
-- [1,3]
filterCat :: (a -> Bool) -> [a] -> [a]
filterCat p = cat listFold (L.prefilter p L.list) . refix
```

## References
[1] Meijer, Erik, Maarten Fokkinga, and Ross Paterson. "Functional programming with bananas, lenses, envelopes and barbed wire." Conference on Functional Programming Languages and Computer Architecture. Springer, Berlin, Heidelberg, 1991.  
[2] [Monadic versions · Issue #5 · vmchale/recursion_schemes](https://github.com/vmchale/recursion_schemes/issues/5)  
[3] [Monoidal Catamorphisms \| Bartosz Milewski's Programming Cafe](https://bartoszmilewski.com/2020/06/15/monoidal-catamorphisms/)  
