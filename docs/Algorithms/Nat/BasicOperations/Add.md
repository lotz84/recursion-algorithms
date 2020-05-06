# add

```hs
module Algorithms.Nat.BasicOperations.Add where

import GHC.Natural

import Data.Functor.Foldable
```

Addition can be represented as catamorphism.

```hs
-- | >>> addCata 1 2
-- 3
addCata :: Natural -> Natural -> Natural
addCata n = cata \case
          Nothing -> n
          Just m  -> 1 + m
```

Mendler-style implementation[^1]

```hs
-- | >>> addMCata 1 2
-- 3
addMCata :: Natural -> Natural -> Natural
addMCata n = mcata f . refix
  where
  f _ Nothing  = n
  f g (Just m) = 1 + g m
```

## References
[1] Uustalu, Tarmo, and Varmo Vene. "Mendler-style inductive types, categorically." Nord. J. Comput. 6.3 (1999): 343.

